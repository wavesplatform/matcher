package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.Monoid
import cats.instances.long._
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
import com.wavesplatform.dex.grpc.integration.clients.state.StatusUpdate.LastBlockHeight
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
import mouse.any._

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class DefaultWavesBlockchainClient(
  meClient: MatcherExtensionClient[Future],
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext, monixScheduler: Scheduler)
    extends WavesBlockchainClient
    with ScorexLogging {

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  private val lastBlockHeight = new AtomicInteger(0)
  @volatile private var isClosing = false

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
      r.updated(x.address.toVanillaAddress, x.out)
    }

  // TODO
  sealed trait CommonUpdate extends Product with Serializable

  object CommonUpdate {
    case class Forged(txIds: Seq[ByteString], knownBalanceUpdated: BlockchainBalance, updatedLastBlockHeight: Option[Int]) extends CommonUpdate
    case class UtxAdded(txs: Seq[UtxTransaction]) extends CommonUpdate // TODO!!! added AND removed by errors
    case class UtxReplaced(newTxs: Seq[UtxTransaction]) extends CommonUpdate
  }

  /**
   * Cases:
   * 1. Downloading blocks: Append+
   * 2. Appending on a network's height: AppendMicro*, RollbackMicro?, Append
   * 2. Rollback: Rollback, Append+
   */
  private def toEvent(event: BlockchainUpdated): Option[BlockchainEvent] = {
    val blockRef = BlockRef(event.height, event.id.toVanilla)
    event.update match {
      case Update.Empty => none // Nothing to do
      case Update.Append(updates) =>
        log.info(s"toEvent.stateUpdate: ${updates.stateUpdate}")
        val regularBalanceChanges = balanceUpdates(updates.stateUpdate).deepReplace(balanceUpdates(updates.transactionStateUpdates))
        val outLeasesChanges = leaseUpdates(updates.stateUpdate).deepCombine(leaseUpdates(updates.transactionStateUpdates))((_, x) => x)
        log.info(s"toEvent.regularBalanceChanges: $regularBalanceChanges")

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

  private def mkBlockchainEventsStream(fromHeight: Int, cancelRef: AtomicReference[Cancelable]): Observable[(BlockchainEvent, Seq[ByteString])] = {
    val (s, c) = bClient.blockchainEvents(fromHeight)
    cancelRef.set(c)
    s
      .map { x =>
        toEvent(x).map { r =>
          val txIds = x.update match {
            case Update.Empty => Seq.empty
            case Update.Append(block) => block.transactionIds
            case _: Update.Rollback => Seq.empty
          }
          (r, txIds)
        }
      }
      .collect { case Some(x) => x }
      .onErrorRecoverWith {
        case e =>
          if (isClosing) Observable.empty
          else {
            val height = Option(lastBlockHeight.get()).map(x => math.max(1, x - 1)).getOrElse(fromHeight)
            log.warn(s"Got an error, subscribing from $height", e)
            val failedEvent = Observable((BlockchainEvent.SyncFailed(height): BlockchainEvent, Seq.empty[ByteString]))
            // TODO wait until connection restored
            Observable(failedEvent, mkBlockchainEventsStream(height, cancelRef)).concat
          }
      }
  }

  // TODO lazy
  override val updates: Observable[Updates] = {
    val bBalances = Observable.fromFuture(meClient.currentBlockInfo).flatMap { startBlockInfo =>
      val startHeight = math.max(startBlockInfo.height - MaxRollbackHeight - 1, 1)

      // TODO Wait until both connections restored!
      val safeStreamCancelable = new AtomicReference(Cancelable.empty)
      val init: BlockchainStatus = BlockchainStatus.Normal(WavesFork(List.empty), startHeight)
      Observable(
        dataUpdates.map((_, Seq.empty[ByteString])),
        mkBlockchainEventsStream(startHeight, safeStreamCancelable)
      ).merge
        .mapAccumulate(init) { case (origStatus, (event, forgedTxIds)) =>
          val x = StatusTransitions(origStatus, event)
          val newLastBlockRef = x.updatedLastBlockHeight match {
            case LastBlockHeight.RestartRequired(at) =>
              safeStreamCancelable.get().cancel()
              at.some
            case LastBlockHeight.Updated(to) => to.some
            case _ => none
          }
          requestBalances(x.requestBalances)
          val r =
            if (x.updatedBalances.isEmpty && forgedTxIds.isEmpty) none[CommonUpdate]
            else CommonUpdate.Forged(
              txIds = forgedTxIds,
              knownBalanceUpdated = x.updatedBalances,
              updatedLastBlockHeight = newLastBlockRef
            ).some
          (x.newStatus, r)
        }
        .collect { case Some(x) => x }
    }

    val meStream = meClient.utxEvents.map { event =>
      event.`type` match {
        case UtxEvent.Type.Switch(event) => CommonUpdate.UtxReplaced(event.transactions).some
        case UtxEvent.Type.Update(event) if event.added.nonEmpty => CommonUpdate.UtxAdded(event.added.flatMap(_.transaction)).some
        case _ => none
      }
    }.collect { case Some(x) => x }

    val finalBalance = mutable.Map.empty[Address, Map[Asset, Long]]
    Observable(meStream, bBalances).merge
      .map { update =>
        // TODO: These changes probably have to be atomic!
        val updated = update match {
          case CommonUpdate.Forged(txIds, knownBalanceUpdated, newHeight) =>
            log.info(
              s"\n---- Forged(txIds=${txIds.map(_.toVanilla).mkString(", ")}, h=${newHeight.fold("s")(_.toString)}, b=$knownBalanceUpdated) ----"
            )
            newHeight.foreach(lastBlockHeight.set)
            val chAddrs = pessimisticPortfolios.processForged(txIds)
            val updated = knownBalances.updateAndGet(_ |+| knownBalanceUpdated)
            val changedAddresses = knownBalanceUpdated.regular.keySet ++ knownBalanceUpdated.outLeases.keySet ++ chAddrs
            val combined = changedAddresses
              .map { address =>
                log.info(s"Forged.combineBalances for $address")
                address -> combineBalances(
                  regular = knownBalanceUpdated.regular.getOrElse(address, Map.empty),
                  outLease = knownBalanceUpdated.outLeases.get(address),
                  pessimistic = Map.empty,
                  updatedRegular = updated.regular.getOrElse(address, Map.empty),
                  updatedOutLease = updated.outLeases.get(address),
                  updatedPessimistic = pessimisticPortfolios.getAggregated(address)
                )
              }
              .toMap

            log.info(s"Combined: $combined\n---- End Forged ----")
            combined

          case CommonUpdate.UtxAdded(txs) =>
            log.info(s"\n---- UtxAdded(${txs.map(_.id.toVanilla).mkString(", ")}) ----")
            pessimisticPortfolios
              .addPending(txs) // Because we remove them during adding a [micro]block
              .map { changedAddress =>
                log.info(s"UtxAdded.combineBalances for $changedAddress")
                changedAddress -> combineBalances(
                  regular = Map.empty,
                  outLease = none,
                  pessimistic = pessimisticPortfolios.getAggregated(changedAddress), // TODO probably some assets weren't changed
                  updatedRegular = knownBalances.get.regular.getOrElse(changedAddress, Map.empty),
                  updatedOutLease = knownBalances.get.outLeases.get(changedAddress),
                  updatedPessimistic = pessimisticPortfolios.getAggregated(changedAddress)
                )
              }.toMap.unsafeTap { _ =>
                log.info("\n----End UtxAdded----")
              }

          case CommonUpdate.UtxReplaced(newTxs) =>
            log.info(s"\n---- UtxReplaced(${newTxs.map(_.id.toVanilla).mkString(", ")}) ----")
            pessimisticPortfolios.replaceWith(newTxs).map { changedAddress =>
              log.info(s"UtxReplaced.combineBalances for $changedAddress")
              changedAddress -> combineBalances(
                regular = Map.empty,
                outLease = none,
                pessimistic = pessimisticPortfolios.getAggregated(changedAddress),
                updatedRegular = knownBalances.get.regular.getOrElse(changedAddress, Map.empty),
                updatedOutLease = knownBalances.get.outLeases.get(changedAddress),
                updatedPessimistic = pessimisticPortfolios.getAggregated(changedAddress)
              )
            }.toMap.unsafeTap { _ =>
              log.info("\n----End UtxReplaced----")
            }
        }

        updated.filter { case (address, updatedBalance) =>
          val prev = finalBalance.getOrElse(address, Map.empty).filter { case (k, _) => updatedBalance.contains(k) }
          val same = prev == updatedBalance
          if (same) log.info(s"Previous balance for $address remains $prev")
          else {
            finalBalance.update(address, prev ++ updatedBalance)
            log.info(s"Changed previous balance for $address from $prev to $updatedBalance")
          }
          !same
        }
      }
      .map(Updates(_))
  }

  // TODO use a smarter approach
  private def requestBalances(x: DiffIndex): Unit =
    if (!x.isEmpty) {
      log.info(s"Request balances: $x")
      meClient.getBalances(x).onComplete {
        case Success(r) =>
          log.info(s"Got balances response: $r")
          dataUpdates.onNext(BlockchainEvent.DataReceived(r))
        case Failure(e) =>
          log.warn("Got an error during requesting balances", e)
          requestBalances(x)
      }
    }

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
      val assetOutLease = outLease.orElse(updatedOutLease).getOrElse(0L)
      val assetPessimistic = pessimistic.get(asset).orElse(updatedPessimistic.get(asset)).getOrElse(0L)
      // TODO solve overflow?
      val r = math.max(0L, assetRegular - assetOutLease + assetPessimistic) // pessimistic is negative
      log.info(s"combineBalances: $asset: r=$r, reg=$assetRegular, ol=$assetOutLease, p=$assetPessimistic")
      asset -> r
    }.toMap
  }

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = {
    val known = knownBalances.get
    val regular = known.regular.getOrElse(address, Map.empty).filter { case (k, _) => assets.contains(k) }
    val outLease = if (assets.contains(Waves)) known.outLeases.get(address) else none
    val blockchainBalance = combineBalances(
      regular = regular,
      outLease = outLease,
      pessimistic = Map.empty,
      updatedRegular = regular,
      updatedOutLease = outLease,
      updatedPessimistic = Map.empty
    )

    val toRequest = assets -- blockchainBalance.keySet
    val response = if (toRequest.isEmpty) Future.successful(Map.empty) else meClient.spendableBalances(address, toRequest)
    response.map { response =>
      (blockchainBalance ++ response) |+| pessimisticPortfolios.getAggregated(address).collect {
        case p @ (asset, _) if assets.contains(asset) => p
      }
    }
  }

  // TODO knownBalances
  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    meClient.allAssetsSpendableBalance(address).map { xs =>
      xs |+| pessimisticPortfolios.getAggregated(address).collect {
        case p @ (asset, _) if xs.keySet.contains(asset) => p
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

  override def close(): Future[Unit] = {
    isClosing = true
    meClient.close().zip(bClient.close()).map(_ => ())
  }

}

object DefaultWavesBlockchainClient {

  val MaxRollbackHeight = 100

}
