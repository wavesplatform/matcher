package com.wavesplatform.dex.grpc.integration.clients

import java.util.concurrent.atomic.AtomicReference

import cats.Monoid
import cats.instances.long._
import cats.instances.queue._
import cats.instances.set._
import cats.syntax.foldable._
import cats.syntax.group._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.CombinedWavesBlockchainClient._
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesClient
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent.WavesNodeUtxEvent
import com.wavesplatform.dex.grpc.integration.clients.domain._
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.clients.matcherext.MatcherExtensionClient
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class CombinedWavesBlockchainClient(
  settings: Settings,
  meClient: MatcherExtensionClient,
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext, monixScheduler: Scheduler)
    extends WavesBlockchainClient
    with ScorexLogging {

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  private val knownBalances: AtomicReference[BlockchainBalance] = new AtomicReference(Monoid.empty[BlockchainBalance])

  private val pessimisticPortfolios = new SynchronizedPessimisticPortfolios(settings.pessimisticPortfolios)

  private val dataUpdates = ConcurrentSubject.publish[WavesNodeEvent]

  override val updates: Observable[Updates] = Observable.fromFuture(meClient.currentBlockInfo).flatMap { startBlockInfo =>
    val startHeight = math.max(startBlockInfo.height - MaxBlockNumberInChain, 1)

    // TODO DEX-1000 Wait until both connections are restored, because one node could be behind another!
    val finalBalance = mutable.Map.empty[Address, Map[Asset, Long]]
    val init: BlockchainStatus = BlockchainStatus.Normal(WavesChain(Vector.empty, startHeight, MaxBlockNumberInChain))

    val combinedStream = new CombinedStream(bClient.blockchainEvents, meClient.utxEvents)
    combinedStream.startFrom(startHeight)

    Observable(dataUpdates, combinedStream.stream)
      .merge
      .mapAccumulate(init) { case (origStatus, event) =>
        val x = StatusTransitions(origStatus, event)
        x.updatedLastBlockHeight match {
          case LastBlockHeight.Updated(to) => combinedStream.updateHeightHint(to)
          case LastBlockHeight.RestartRequired(from) => combinedStream.restartFrom(from)
          case _ =>
        }
        if (x.requestNextBlockchainEvent) bClient.blockchainEvents.requestNext()
        requestBalances(x.requestBalances)
        val finalKnownBalances = knownBalances.updateAndGet(_ |+| x.updatedBalances)
        val updatedPessimistic = processUtxEvents(x.processUtxEvents)
        val changedAddresses = finalKnownBalances.regular.keySet ++ finalKnownBalances.outLeases.keySet ++ updatedPessimistic
        val updatedFinalBalances = changedAddresses
          .map { address =>
            address -> combineBalances(
              updatedRegular = x.updatedBalances.regular.getOrElse(address, Map.empty),
              updatedOutLease = x.updatedBalances.outLeases.get(address).fold(Map.empty[Asset, Long])(x => Map(Waves -> x)),
              // TODO DEX-1013
              updatedPessimistic = if (updatedPessimistic.contains(address)) pessimisticPortfolios.getAggregated(address) else Map.empty,
              finalRegular = finalKnownBalances.regular.getOrElse(address, Map.empty),
              finalOutLease = finalKnownBalances.outLeases.get(address).fold(Map.empty[Asset, Long])(x => Map(Waves -> x)),
              finalPessimistic = pessimisticPortfolios.getAggregated(address)
            )
          }
          .toMap
        (x.newStatus, updatedFinalBalances)
      }
      .filter(_.nonEmpty)
      .map { updated => // TODO DEX-1014
        updated.filter { case (address, updatedBalance) =>
          val prev = finalBalance.getOrElse(address, Map.empty).filter { case (k, _) => updatedBalance.contains(k) }
          val same = prev == updatedBalance
          if (!same) finalBalance.update(address, prev ++ updatedBalance)
          !same
        }
      }
      .doOnError(e => Task(log.error("Got an error in the combined stream", e)))
      .map(Updates(_))
  }

  private def processUtxEvents(queue: Queue[WavesNodeUtxEvent]): Set[Address] = queue.foldMap(processUtxEvent)

  // TODO DEX-1013
  private def processUtxEvent(event: WavesNodeUtxEvent): Set[Address] = event match {
    case WavesNodeUtxEvent.Updated(newTxs, failedTxs) =>
      pessimisticPortfolios.addPending(newTxs) |+| pessimisticPortfolios.removeFailed(failedTxs.map(_.id))
    case WavesNodeUtxEvent.Forged(txIds) => pessimisticPortfolios.processForged(txIds)._1
    case WavesNodeUtxEvent.Switched(newTxs) => pessimisticPortfolios.replaceWith(newTxs)
  }

  // TODO DEX-1015
  private def requestBalances(x: DiffIndex): Unit =
    if (!x.isEmpty)
      meClient.getBalances(x).onComplete {
        case Success(r) => dataUpdates.onNext(WavesNodeEvent.DataReceived(r))
        case Failure(e) =>
          log.warn("Got an error during requesting balances", e)
          requestBalances(x)
      }

  private def combineBalances(
    updatedRegular: Map[Asset, Long],
    updatedOutLease: Map[Asset, Long],
    updatedPessimistic: Map[Asset, Long],
    finalRegular: Map[Asset, Long],
    finalOutLease: Map[Asset, Long],
    finalPessimistic: Map[Asset, Long]
  ): Map[Asset, Long] = {
    val changedAssets = updatedRegular.keySet ++ updatedPessimistic.keySet ++ updatedOutLease.keySet
    changedAssets.map { asset =>
      val assetRegular = updatedRegular.get(asset).orElse(finalRegular.get(asset)).getOrElse(0L)
      val assetOutLease = updatedOutLease.get(asset).orElse(finalOutLease.get(asset)).getOrElse(0L)
      val assetPessimistic = updatedPessimistic.get(asset).orElse(finalPessimistic.get(asset)).getOrElse(0L)
      // TODO solve overflow?
      val r = math.max(0L, assetRegular - assetOutLease + assetPessimistic) // pessimistic is negative
      asset -> r
    }.toMap
  }

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = {
    val known = knownBalances.get
    val regular = known.regular.getOrElse(address, Map.empty).filter { case (k, _) => assets.contains(k) }
    val blockchainBalance = combineBalances(
      updatedRegular = regular,
      updatedOutLease = Map.empty,
      updatedPessimistic = Map.empty,
      finalRegular = Map.empty,
      // Will be used only if Waves in assets, see combineBalances
      finalOutLease = known.outLeases.get(address).fold(Map.empty[Asset, Long])(x => Map(Waves -> x)),
      finalPessimistic = Map.empty
    )

    val toRequest = assets -- blockchainBalance.keySet
    val response = if (toRequest.isEmpty) Future.successful(Map.empty) else meClient.spendableBalances(address, toRequest)
    response.map { response =>
      (blockchainBalance ++ response) |+| pessimisticPortfolios.getAggregated(address).collect {
        case p @ (asset, _) if assets.contains(asset) => p
      }
    }
  }

  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    meClient.allAssetsSpendableBalance(address).map { xs =>
      xs |+| pessimisticPortfolios.getAggregated(address).collect {
        case p @ (asset, _) if xs.keySet.contains(asset) => p
      }
    }

  // TODO DEX-1012
  override def isFeatureActivated(id: Short): Future[Boolean] =
    meClient.isFeatureActivated(id)

  // TODO DEX-353
  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] =
    meClient.assetDescription(asset)

  // TODO DEX-353
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

  override def close(): Future[Unit] =
    meClient.close().zip(bClient.close()).map(_ => ())

}

object CombinedWavesBlockchainClient {

  val MaxBlockNumberInChain = 101 // MaxRollBackHeight is 100

  case class Settings(pessimisticPortfolios: SynchronizedPessimisticPortfolios.Settings)

}
