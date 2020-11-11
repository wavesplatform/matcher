package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.Monoid
import cats.instances.list._
import cats.instances.long._
import cats.instances.set._
import cats.syntax.foldable._
import cats.syntax.group._
import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.collection.MapOps.{Ops, Ops2}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.DefaultWavesBlockchainClient._
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.clients.state.StatusUpdate.HeightUpdate
import com.wavesplatform.dex.grpc.integration.clients.state._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.{UtxEvent, UtxTransaction}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object WavesBlockchainClient {

  type BalanceChanges = Map[Address, Map[Asset, Long]]
  case class Updates(updatedBalances: Map[Address, Map[Asset, Long]])

  object Updates {

    implicit val updatesMonoid: Monoid[Updates] = new Monoid[Updates] {
      override val empty: Updates = Updates(Map.empty)

      override def combine(x: Updates, y: Updates): Updates = Updates(
        x.updatedBalances.deepReplace(y.updatedBalances)
      )

    }

  }

}

trait WavesBlockchainClient {
  def updates: Observable[WavesBlockchainClient.Updates]

  def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]]

  def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): Future[Boolean]

  def forgedOrder(orderId: ByteStr): Future[Boolean]

  def getNodeAddress: Future[InetAddress]

  def close(): Future[Unit]
}

class DefaultWavesBlockchainClient(
  meClient: MatcherExtensionClient[Future],
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext, monixScheduler: Scheduler)
    extends WavesBlockchainClient
    with ScorexLogging {

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  private val currentMaxHeight = new AtomicInteger(0)

  private val emptyBalances: Balances = Map.empty
  private val knownBalances: AtomicReference[BlockchainBalance] = new AtomicReference(Monoid.empty[BlockchainBalance])

  private val pessimisticPortfolios = new PessimisticPortfolios

  private val dataUpdates = ConcurrentSubject.publish[BlockchainEvent]

  // TODO replace with deepReplace ?
  private def balanceUpdates(stateUpdate: Iterable[StateUpdate]): Balances =
    stateUpdate.flatMap(_.balances).foldLeft(emptyBalances) {
      case (r, x) =>
        // TODO what if absent? All assets has gone?
        x.amount.fold(r) { assetAmount =>
          val address = x.address.toVanillaAddress
          val updated = r
            .getOrElse(address, Map.empty)
            .updated(assetAmount.assetId.toVanillaAsset, assetAmount.amount)
          log.info(s"balanceUpdates: $address: ${assetAmount.assetId.toVanillaAsset} -> ${assetAmount.amount}, updated: $updated")
          r.updated(address, updated)
        }
    }

  private def leaseUpdates(stateUpdate: Iterable[StateUpdate]): Leases =
    stateUpdate.flatMap(_.leases).foldLeft[Leases](Map.empty) { case (r, x) =>
      if (x.out <= 0) r
      else r.updated(x.address.toVanillaAddress, x.out)
    }

  /**
   * Cases:
   * 1. Downloading blocks: Append+
   * 2. Appending on a network's height: AppendMicro*, RollbackMicro?, Append
   * 2. Rollback: Rollback, Append+
   */
  private def mkEventsStream(orig: Observable[BlockchainUpdated]): Observable[BlockchainEvent] = orig
    .map { event =>
      val blockRef = BlockRef(event.height, event.id.toVanilla)
      event.update match {
        case Update.Empty => none // Nothing to do
        case Update.Append(updates) =>
          log.info(s"mkEventsStream.stateUpdate: ${updates.stateUpdate}")
          val regularBalanceChanges = balanceUpdates(updates.stateUpdate).deepReplace(balanceUpdates(updates.transactionStateUpdates))
          val outLeasesChanges = leaseUpdates(updates.stateUpdate).deepCombine(leaseUpdates(updates.transactionStateUpdates))((_, x) => x)
          log.info(s"mkEventsStream.regularBalanceChanges: $regularBalanceChanges")

          val blockInfo = updates.body match {
            case Body.Empty => none // Log
            case Body.Block(block) => (WavesBlock.Type.Block, block.block.get.header.get.reference.toVanilla).some
            case Body.MicroBlock(block) => (WavesBlock.Type.MicroBlock, block.microBlock.get.microBlock.get.reference.toVanilla).some
          }

          blockInfo.map { case (tpe, reference) =>
            val block = WavesBlock(
              ref = blockRef,
              reference = reference,
              changes = BlockchainBalance(regularBalanceChanges, outLeasesChanges),
              tpe = tpe
            )

            BlockchainEvent.Appended(block)
          }

        case Update.Rollback(value) =>
          value.`type` match {
            case RollbackType.BLOCK | RollbackType.MICROBLOCK => BlockchainEvent.RolledBackTo(blockRef).some
            case RollbackType.Unrecognized(_) => none // TODO ???
          }
      }
    }
    .collect { case Some(x) => x }

  // TODO lazy
  override val updates: Observable[Updates] = {
    val bBalances = Observable.fromFuture(meClient.currentBlockInfo).flatMap { startBlockInfo =>
      val start = math.max(startBlockInfo.height - MaxRollbackHeight - 1, 1)
      currentMaxHeight.set(start)

      @volatile var safeStreamCancelable = Cancelable.empty
      val safeStream = {
        val (s, c) = bClient.blockchainEvents(start)
        safeStreamCancelable = c
        s.onErrorRecoverWith {
          case _ =>
            val (s, c) = bClient.blockchainEvents(currentMaxHeight.get())
            safeStreamCancelable = c
            s
        }
      }

      // TODO cache addresses
      val init: BlockchainStatus = BlockchainStatus.Normal(WavesFork(List.empty), start)
      Observable(mkEventsStream(safeStream), dataUpdates).merge
        .mapAccumulate(init) { (origStatus, event) =>
          val x = StatusTransitions(origStatus, event)
          val updatedBalances = x.updatedHeight match {
            case HeightUpdate.RestartRequired(atHeight) =>
              currentMaxHeight.set(atHeight)
              safeStreamCancelable.cancel()
              emptyBalances
            case HeightUpdate.Updated(newHeight) =>
              currentMaxHeight.set(newHeight)
              runSideEffects(x)
              collectBalanceChanges(x)
            case _ =>
              runSideEffects(x)
              collectBalanceChanges(x)
          }
          (x.newStatus, updatedBalances)
        }
        .filter(_.nonEmpty)
    }

    val meStream = meClient.utxEvents.map { event =>
      event.`type` match {
        case UtxEvent.Type.Switch(event) =>
          pessimisticPortfolios.replaceWith(event.transactions.toSet).map { changedAddress =>
            changedAddress -> combineBalances(
              regular = Map.empty,
              outLease = none,
              pessimistic = pessimisticPortfolios.getAggregated(changedAddress),
              updatedRegular = knownBalances.get.regular.getOrElse(changedAddress, Map.empty),
              updatedOutLease = knownBalances.get.outLeases.get(changedAddress),
              updatedPessimistic = pessimisticPortfolios.getAggregated(changedAddress)
            )
          }.toMap
        // TODO
        case UtxEvent.Type.Update(event) =>
          pessimisticPortfolios
            .addAndRemove(
              addTxs = event.added.flatMap(_.transaction),
              removeTxs = event.removed.flatMap(_.transaction)
            )
            .map { changedAddress =>
              changedAddress -> combineBalances(
                regular = Map.empty,
                outLease = none,
                pessimistic = pessimisticPortfolios.getAggregated(changedAddress),
                updatedRegular = knownBalances.get.regular.getOrElse(changedAddress, Map.empty),
                updatedOutLease = knownBalances.get.outLeases.get(changedAddress),
                updatedPessimistic = pessimisticPortfolios.getAggregated(changedAddress)
              )
            }.toMap
        case _ => Map.empty[Address, Map[Asset, Long]]
      }
    }

    Observable(bBalances, meStream).merge.map(Updates(_))
  }

  private def runSideEffects(x: StatusUpdate): Unit =
    if (!x.requestBalances.isEmpty)
      log.warn(s"Request balances: ${x.requestBalances}") // TODO

  private def combineBalances(
    regular: Map[Asset, Long],
    outLease: Option[Long],
    pessimistic: Map[Asset, Long],
    updatedRegular: Map[Asset, Long],
    updatedOutLease: Option[Long],
    updatedPessimistic: Map[Asset, Long]
  ): Map[Asset, Long] = {
    val changedAssets = regular.keySet ++ pessimistic.keySet ++ outLease.map(_ => Waves)
    changedAssets.map { asset =>
      val assetRegular = regular.get(asset).orElse(updatedRegular.get(asset)).getOrElse(0L)
      val assetLeaseOut = outLease.orElse(updatedOutLease).getOrElse(0L)
      val assetPessimistic = pessimistic.get(asset).orElse(updatedPessimistic.get(asset)).getOrElse(0L)
      asset -> math.max(0L, assetRegular - assetLeaseOut + assetPessimistic) // pessimistic is negative
    }.toMap
  }

  // TODO combine three Map[Asset, Long] for each address
  private def collectBalanceChanges(x: StatusUpdate): Map[Address, Map[Asset, Long]] =
    if (x.updatedBalances.isEmpty) emptyBalances
    else {
      val updated = knownBalances.updateAndGet(_ |+| x.updatedBalances)
      val changedAddresses = x.updatedBalances.regular.keySet ++ x.updatedBalances.outLeases.keySet
      val combined = changedAddresses
        .map { address =>
          address -> combineBalances(
            regular = x.updatedBalances.regular.getOrElse(address, Map.empty),
            outLease = x.updatedBalances.outLeases.get(address),
            pessimistic = Map.empty,
            updatedRegular = updated.regular.getOrElse(address, Map.empty),
            updatedOutLease = updated.outLeases.get(address),
            updatedPessimistic = pessimisticPortfolios.getAggregated(address)
          )
        }
        .toMap

      log.info(s"Got HeightUpdate.Updated $x\nCombined: $combined")

      combined
    }

  // TODO knownBalances
  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    meClient.spendableBalances(address, assets).map(_ |+| pessimisticPortfolios.getAggregated(address).collect {
      case p @ (asset, v) if assets.contains(asset) => p
    })

  // TODO knownBalances
  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    meClient.allAssetsSpendableBalance(address).map { xs =>
      xs |+| pessimisticPortfolios.getAggregated(address).collect {
        case p @ (asset, v) if xs.keySet.contains(asset) => p
      }
    }

  // TODO get all and track in bClient
  override def isFeatureActivated(id: Short): Future[Boolean] =
    meClient.isFeatureActivated(id)

  // TODO track?
  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] =
    meClient.assetDescription(asset)

  // TODO track?
  override def hasScript(asset: IssuedAsset): Future[Boolean] = meClient.hasScript(asset)

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] =
    meClient.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] =
    meClient.hasScript(address)

  override def runScript(address: Address, input: Order): Future[RunScriptResult] =
    meClient.runScript(address, input)

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] =
    meClient.wereForged(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean] =
    meClient.broadcastTx(tx)

  override def forgedOrder(orderId: ByteStr): Future[Boolean] =
    meClient.forgedOrder(orderId)

  override def getNodeAddress: Future[InetAddress] =
    meClient.getNodeAddress

  override def close(): Future[Unit] = meClient.close().zip(bClient.close()).map(_ => ())

}

object DefaultWavesBlockchainClient {

  val MaxRollbackHeight = 100

  private class PessimisticPortfolios extends ScorexLogging {

    private val reentrantLock = new ReentrantReadWriteLock()

    private def read[T](f: => T): T =
      try { reentrantLock.readLock().lock(); f }
      finally reentrantLock.readLock().unlock()

    private def write[T](f: => T): T =
      try { reentrantLock.writeLock().lock(); f }
      finally reentrantLock.writeLock().unlock()

    // Longs are negative in both maps, see getPessimisticPortfolio
    private val portfolios = new mutable.AnyRefMap[Address, Map[Asset, Long]]()
    private val txs = new mutable.AnyRefMap[ByteString, Map[Address, Map[Asset, Long]]] // TODO id <------------

    def replaceWith(setTxs: Set[UtxTransaction]): Set[Address] = write {
      val setTxMap = setTxs.map(tx => tx.id -> tx).toMap
      val oldTxIds = txs.keySet.toSet -- setTxMap.keySet
      val newTxIds = setTxMap.keySet -- txs.keySet

      val newTxsPortfolios = newTxIds.toList.map(id => id -> getPessimisticPortfolio(setTxMap(id))) // TODO apply
      newTxsPortfolios.foreach(Function.tupled(txs.put))

      val addPortfolios = newTxsPortfolios.foldMap(_._2)
      val subtractPortfolios = oldTxIds.toList.foldMap(txs.remove(_).getOrElse(Map.empty))
      val diff = addPortfolios |-| subtractPortfolios
      diff.foreach { case (address, diff) =>
        portfolios.updateWith(address) { prev =>
          (prev.getOrElse(Map.empty) |+| diff)
            .filter(_._2 < 0) // TODO: guess it is not possible, but...
            .some
        }
      }
      log.info(s"replaceWith ids=${setTxMap.keySet.map(_.toVanilla)}, diff=$diff, txs=$setTxs")
      diff.keySet
    }

    // TODO
    def addAndRemove(addTxs: Seq[UtxTransaction], removeTxs: Seq[UtxTransaction]): Set[Address] = write {
      log.info(s"addTxs: ${addTxs.map(_.id.toVanilla)}, removeTxs: ${removeTxs.map(_.id.toVanilla)}")
      addTxs.toList.foldMapK[Set, Address](addUnsafe) |+|
      removeTxs.toList.foldMapK[Set, Address](removeUnsafe)
    }

    private def addUnsafe(tx: UtxTransaction): Set[Address] = {
      val id = tx.id
      if (txs.contains(id)) {
        log.info(s"addUnsafe: already has ${id.toVanilla}")
        Set.empty
      } else {
        val finalP = getPessimisticPortfolio(tx)
        log.info(s"addUnsafe: id=${id.toVanilla}, diff=$finalP, tx=$tx")
        // TODO we calculate and check only in the and?
        if (txs.put(id, finalP).isEmpty) {
          finalP.foreach {
            case (address, p) => portfolios.updateWith(address)(prev => (prev.getOrElse(Map.empty) |+| p).some)
          }
          finalP.keySet
        } else Set.empty
      }
    }

    private def removeUnsafe(tx: UtxTransaction): Set[Address] =
      txs.remove(tx.id) match {
        case None =>
          log.info(s"removeUnsafe: wasn't id=${tx.id.toVanilla}, tx=$tx")
          Set.empty[Address]
        case Some(p) =>
          log.info(s"removeUnsafe: id=${tx.id.toVanilla}, diff=$p, tx=$tx")
          p.foreach {
            case (address, p) =>
              portfolios.updateWith(address) { prev =>
                prev.map(_ |-| p) // TODO cleanup. sometimes?
              }
          }
          p.keySet
      }

    def getAggregated(address: Address): Map[Asset, Long] = read(portfolios.getOrElse(address, Map.empty))

    // Utility

    def getPessimisticPortfolio(tx: UtxTransaction): Map[Address, Map[Asset, Long]] = tx.diff.flatMap(_.stateUpdate)
      .fold(Map.empty[Address, Map[Asset, Long]]) { diff =>
        // Balances
        val p1 = diff.balances.groupBy(_.address).map {
          case (address, updates) =>
            val balances = updates.view
              .flatMap(_.amount)
              .collect {
                case x if x.amount < 0 => x.assetId.toVanillaAsset -> x.amount // pessimistic
              }
              .toMap
            address.toVanillaAddress -> balances
        }

        // Leasing
        val finalP = diff.leases.foldLeft(p1) {
          case (r, x) =>
            if (x.out <= 0) r // pessimistic
            else {
              val address = x.address.toVanillaAddress
              val orig = r.getOrElse(address, Map.empty)
              val updated = orig.updated(Waves, orig.getOrElse(Waves, 0L) - x.out)
              r.updated(address, updated)
            }
        }

        finalP
      }

  }

}
