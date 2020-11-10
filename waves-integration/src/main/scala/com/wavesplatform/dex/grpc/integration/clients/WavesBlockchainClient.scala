package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.Monoid
import cats.syntax.group._
import cats.syntax.option._
import com.wavesplatform.dex.collection.MapOps.{Ops, Ops2}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.DefaultWavesBlockchainClient._
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.clients.state.StatusUpdate.HeightUpdate
import com.wavesplatform.dex.grpc.integration.clients.state._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

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

//  private val pessimisticPortfolios = new PessimisticPortfolios

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
              updateAll(x)
            case _ =>
              updateAll(x)
          }
          (x.newStatus, updatedBalances)
        }
        .filter(_.nonEmpty)
    }

    /*val meStream = meClient.utxEvents.map { event =>
      event.`type` match {
        case Type.Switch(event) => event.transaction.map()
        case Type.Update(event) =>
        case Type.Empty =>
      }
    }*/

    // Observable(bState, meStream).merge.map(Updates)
    bBalances.map(Updates(_))
  }

  private def updateAll(x: StatusUpdate): Map[Address, Map[Asset, Long]] = {
    if (!x.requestBalances.isEmpty)
      log.warn(s"Request balances: ${x.requestBalances}") // TODO
    if (x.updatedBalances.isEmpty) emptyBalances
    else {
      val updated = knownBalances.updateAndGet(_ |+| x.updatedBalances)

      val addressesWithChangedWaves = x.updatedBalances.regular.foldLeft(List.empty[Address]) {
        case (r, (address, assets)) =>
          if (assets.contains(Waves)) address :: r
          else r
      }

      val changedOutLeasesAddresses = x.updatedBalances.outLeases.keySet
      val origLeases = addressesWithChangedWaves.view
        .filterNot(changedOutLeasesAddresses.contains)
        .map(address => address -> updated.outLeases.getOrElse(address, 0L))
        .toMap

      val combined = (origLeases ++ x.updatedBalances.outLeases).foldLeft(x.updatedBalances.regular) {
        case (r, (address, updatedOutLease)) =>
          val origAddressBalance = r.get(address)
          val origWavesBalance = origAddressBalance.flatMap(_.get(Waves))
            .orElse(updated.regular.get(address).flatMap(_.get(Waves)))
            .getOrElse(0L)

          val wavesBalance = math.max(0L, origWavesBalance - updatedOutLease)
          r.updated(address, origAddressBalance.getOrElse(Map.empty).updated(Waves, wavesBalance))
      }

      log.info(s"Got HeightUpdate.Updated $x\nCombined: $combined")

      combined
    }
  }

  // TODO knownBalances
  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    meClient.spendableBalances(address, assets)

  // TODO knownBalances
  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    meClient.allAssetsSpendableBalance(address)

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

  private def upsert(orig: Balances, update: Balances): Balances =
    update.foldLeft(orig) { case (r, (address, balances)) =>
      val orig = r.getOrElse(address, Map.empty)
      r.updated(address, orig ++ balances)
    }

}

object DefaultWavesBlockchainClient {

  val MaxRollbackHeight = 100

//  private class PessimisticPortfolios {
//
//    private type Portfolios = Map[Address, Map[Asset, Long]]
//
//    private val portfolios = new ConcurrentHashMap[Address, Map[Asset, Long]]()
//    private val txs = TrieMap.empty[UtxTransaction, Portfolios] // TODO Use ID
//
//    def add(tx: UtxTransaction): Unit = tx.diff.flatMap(_.stateUpdate).foreach { diff =>
//      // Balances
//      val p1: Portfolios = diff.balances.groupBy(_.address).map {
//        case (address, updates) =>
//          val balances = updates.view
//            .flatMap(_.amount)
//            .collect {
//              case x if x.amount < 0 => x.assetId.toVanillaAsset -> x.amount // pessimistic
//            }
//            .toMap
//          address.toVanillaAddress -> balances
//      }
//
//      // Leasing
//      val finalP: Portfolios = diff.leases.foldLeft(p1) {
//        case (r, x) =>
//          if (x.out <= 0) r // pessimistic
//          else {
//            val address = x.address.toVanillaAddress
//            val orig = r.getOrElse(address, Map.empty)
//            val updated = orig.updated(Waves, orig.getOrElse(Waves, 0L) - x.out)
//            r.updated(address, updated)
//          }
//      }
//
//      if (txs.putIfAbsent(tx, finalP).isEmpty)
//        finalP.foreach {
//          case (address, balances) => portfolios.compute(address, (_, orig) => orig |+| balances)
//        }
//    }
//
//    def remove(tx: UtxTransaction): Unit = txs.remove(tx).foreach { p =>
//      p.foreach {
//        case (address, balances) => portfolios.compute(address, (_, orig) => orig |-| balances)
//      }
//    }
//
//    def getAggregated(address: Address): Map[Asset, Long] = portfolios.get(address)
//
//  }

}
