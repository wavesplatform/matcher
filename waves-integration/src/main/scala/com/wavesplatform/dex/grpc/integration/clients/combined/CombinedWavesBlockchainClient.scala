package com.wavesplatform.dex.grpc.integration.clients.combined

import java.util.concurrent.atomic.AtomicReference

import cats.Monoid
import cats.instances.long._
import cats.instances.set._
import cats.syntax.group._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.BlockchainUpdatesClient
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedWavesBlockchainClient._
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.domain._
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.clients.matcherext.MatcherExtensionClient
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.clients.{RunScriptResult, WavesBlockchainClient}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining._
import scala.util.{Failure, Success}
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.protobuf.transaction.SignedTransaction

class CombinedWavesBlockchainClient(
  settings: Settings,
  matcherPublicKey: PublicKey, // * @param matcherPublicKey Used to ignore transactions other than Matcher's ones in Updates
  meClient: MatcherExtensionClient,
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext, monixScheduler: Scheduler)
    extends WavesBlockchainClient
    with ScorexLogging {

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  private val pbMatcherPublicKey = matcherPublicKey.toPB

  private val knownBalances: AtomicReference[BlockchainBalance] = new AtomicReference(Monoid.empty[BlockchainBalance])

  private val pessimisticPortfolios = new SynchronizedPessimisticPortfolios(settings.pessimisticPortfolios)

  private val dataUpdates = ConcurrentSubject.publish[WavesNodeEvent]

  override val updates: Observable[Updates] = Observable.fromFuture(meClient.currentBlockInfo)
    .flatMap { startBlockInfo =>
      log.info(s"Current block: $startBlockInfo")
      val startHeight = math.max(startBlockInfo.height - settings.maxRollbackHeight - 1, 1)

      val finalBalance = mutable.Map.empty[Address, Map[Asset, Long]]
      val init: BlockchainStatus = BlockchainStatus.Normal(WavesChain(Vector.empty, startHeight, settings.maxRollbackHeight + 1))

      val combinedStream = new CombinedStream(settings.combinedStream, bClient.blockchainEvents, meClient.utxEvents)
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
          val updatedPessimistic = processUtxEvents(x.utxUpdate)
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
          (
            x.newStatus,
            Updates(
              updatedFinalBalances,
              appearedTxs = ({
                x.utxUpdate.forgedTxs.view.collect { case (id, x) if isExchangeTransactionFromMatcher(x.tx) => id.toVanilla -> x }
              } ++ {
                for {
                  tx <- x.utxUpdate.unconfirmedTxs.view if isExchangeTransactionFromMatcher(tx)
                  signedTx <- tx.transaction
                  changes <- tx.diff.flatMap(_.stateUpdate)
                } yield tx.id.toVanilla -> TransactionWithChanges(signedTx, changes)
              }).toMap,
              failedTxs = (for {
                tx <- x.utxUpdate.failedTxs.values if isExchangeTransactionFromMatcher(tx)
                signedTx <- tx.transaction
                changes <- tx.diff.flatMap(_.stateUpdate)
              } yield tx.id.toVanilla -> TransactionWithChanges(signedTx, changes)).toMap
            )
          )
        }
        .filterNot(_.isEmpty)
        .map { updated => // TODO DEX-1014
          updated.copy(
            updatedBalances = updated.updatedBalances.filter { case (address, updatedBalance) =>
              val prev = finalBalance.getOrElse(address, Map.empty).filter { case (k, _) => updatedBalance.contains(k) }
              val different = prev != updatedBalance
              if (different) finalBalance.update(address, prev ++ updatedBalance)
              different
            }
          )
        }
        .tap(_ => combinedStream.startFrom(startHeight))
    }
    .doOnError(e => Task(log.error("Got an error in the combined stream", e)))

  // TODO DEX-1013
  private def processUtxEvents(utxUpdate: UtxUpdate): Set[Address] =
    if (utxUpdate.resetCaches) pessimisticPortfolios.replaceWith(utxUpdate.unconfirmedTxs)
    else
      pessimisticPortfolios.addPending(utxUpdate.unconfirmedTxs) |+|
      pessimisticPortfolios.processForged(utxUpdate.forgedTxs.keySet)._1 |+|
      pessimisticPortfolios.removeFailed(utxUpdate.failedTxs.keySet) // TODO Not only exchange transactions

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

  private def isExchangeTransactionFromMatcher(tx: SignedTransaction): Boolean =
    tx.transaction.exists { tx =>
      tx.data.isExchange && ByteString.unsignedLexicographicalComparator().compare(tx.senderPublicKey, pbMatcherPublicKey) == 0
    }

  private def isExchangeTransactionFromMatcher(tx: UtxTransaction): Boolean = tx.transaction.exists(isExchangeTransactionFromMatcher)

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

  override def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] =
    meClient.areKnown(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean] =
    meClient.broadcastTx(tx)

  override def isOrderForged(orderId: ByteStr): Future[Boolean] =
    meClient.forgedOrder(orderId)

  override def close(): Future[Unit] =
    meClient.close().zip(bClient.close()).map(_ => ())

}

object CombinedWavesBlockchainClient {

  case class Settings(
    maxRollbackHeight: Int,
    combinedStream: CombinedStream.Settings,
    pessimisticPortfolios: SynchronizedPessimisticPortfolios.Settings
  )

}
