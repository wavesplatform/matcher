package com.wavesplatform.dex.grpc.integration.clients.combined

import cats.instances.future._
import cats.instances.set._
import cats.syntax.contravariantSemigroupal._
import cats.syntax.group._
import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.collections.ListOps.ListOfMapsOps
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.{BlockchainUpdatesClient, DefaultBlockchainUpdatesClient}
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status.Starting
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedWavesBlockchainClient.Settings
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.domain._
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.clients.matcherext.{MatcherExtensionCachingClient, MatcherExtensionClient, MatcherExtensionGrpcAsyncClient}
import com.wavesplatform.dex.grpc.integration.clients.{BroadcastResult, CheckedBroadcastResult, RunScriptResult, WavesBlockchainClient}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import com.wavesplatform.dex.grpc.integration.tool.RestartableManagedChannel
import com.wavesplatform.protobuf.transaction.SignedTransaction
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining._
import scala.util.{Failure, Success}

class CombinedWavesBlockchainClient(
  settings: Settings,
  matcherPublicKey: PublicKey,
  meClient: MatcherExtensionClient,
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext, monixScheduler: Scheduler)
    extends WavesBlockchainClient
    with ScorexLogging {

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  @volatile private var blockchainStatus: Status = Starting()

  override def status(): Status = blockchainStatus

  private val pbMatcherPublicKey = matcherPublicKey.toPB

  private val pessimisticPortfolios = new SynchronizedPessimisticPortfolios(settings.pessimisticPortfolios)

  private val dataUpdates = ConcurrentSubject.publish[WavesNodeEvent]

  // HACK: NODE: We need to store last updates and consider them as fresh, because we can face an issue during fullBalancesSnapshot
  //   when balance changes were deleted from LiquidBlock's diff, but haven't yet saved to DB
  @volatile private var lastUpdates = List.empty[BlockchainBalance]
  private val maxPreviousBlockUpdates = settings.maxCachedLatestBlockUpdates - 1

  override lazy val updates: Observable[(WavesNodeUpdates, Boolean)] = Observable.fromFuture(meClient.currentBlockInfo)
    .flatMap { startBlockInfo =>
      log.info(s"Current block: $startBlockInfo")
      val startHeight = math.max(startBlockInfo.height - settings.maxRollbackHeight - 1, 1)
      val init: BlockchainStatus = BlockchainStatus.Normal(WavesChain(Vector.empty, startHeight - 1, settings.maxRollbackHeight + 1))

      val combinedStream = new CombinedStream(settings.combinedStream, bClient.blockchainEvents, meClient.utxEvents)

      Observable(dataUpdates, combinedStream.stream)
        .merge
        .mapAccumulate(init) { case (origStatus, event) =>
          val x = StatusTransitions(origStatus, event)
          x.updatedLastBlockHeight match {
            case LastBlockHeight.Updated(to) => combinedStream.updateProcessedHeight(to)
            case LastBlockHeight.RestartRequired => combinedStream.restart()
            case _ =>
          }
          if (x.requestNextBlockchainEvent) bClient.blockchainEvents.requestNext()
          requestBalances(x.requestBalances)
          val updatedPessimistic = processUtxEvents(x.utxUpdate) // TODO DEX-1045 Do we need to filter out known transactions (WavesChain)?
          val changedAddresses = x.updatedBalances.regular.keySet ++ x.updatedBalances.outgoingLeasing.keySet ++ updatedPessimistic
          val updatedFinalBalances = changedAddresses
            .map { address =>
              address -> AddressBalanceUpdates(
                regular = x.updatedBalances.regular.getOrElse(address, Map.empty),
                outgoingLeasing = x.updatedBalances.outgoingLeasing.get(address),
                pessimisticCorrection =
                  if (updatedPessimistic.contains(address)) pessimisticPortfolios.getAggregated(address)
                  else Map.empty
              )
            }
            .toMap

          // It is safe even we are during a rollback, because StatusTransitions doesn't propagate data until fork is resolved
          if (!x.updatedBalances.isEmpty) lastUpdates = x.updatedBalances :: lastUpdates.take(maxPreviousBlockUpdates)

          // // Not useful for UTX, because it doesn't consider the current state of orders
          // // Will be useful, when blockchain updates send order fills.
          // val fillsDebugInfo = x.utxUpdate.unconfirmedTxs.flatMap { tx =>
          //   val fills = tx.diff.toList.flatMap(_.orderFills).map { fill =>
          //     s"${fill.orderId.toVanilla} -> v:${fill.volume} + f:${fill.fee}"
          //   }
          //   if (fills.isEmpty) Nil
          //   else List(s"${tx.id.toVanilla}: ${fills.mkString(", ")}")
          // }
          //
          // if (fillsDebugInfo.nonEmpty) log.info(s"Detected fills:\n${fillsDebugInfo.mkString("\n")}")

          val unconfirmedTxs = for {
            tx <- x.utxUpdate.unconfirmedTxs.view if isExchangeTxFromMatcher(tx)
            signedTx <- tx.transaction
            changes <- tx.diff.flatMap(_.stateUpdate)
          } yield tx.id.toVanilla -> TransactionWithChanges(tx.id, signedTx, changes)

          val confirmedTxs = x.utxUpdate.confirmedTxs.view
            .collect { case (id, x) if isExchangeTxFromMatcher(x.tx) => id.toVanilla -> x }

          val failedTxs = for {
            tx <- x.utxUpdate.failedTxs.values.view if isExchangeTxFromMatcher(tx)
            signedTx <- tx.transaction
            changes <- tx.diff.flatMap(_.stateUpdate)
          } yield tx.id.toVanilla -> TransactionWithChanges(tx.id, signedTx, changes)

          blockchainStatus = combinedStream.status()

          val updates = WavesNodeUpdates(updatedFinalBalances, (unconfirmedTxs ++ confirmedTxs ++ failedTxs).toMap)
          (x.newStatus, (updates, combinedStream.currentProcessedHeight >= startBlockInfo.height))
        }
        .filterNot(_._1.isEmpty)
        .tap(_ => combinedStream.startFrom(startHeight))

    }
    .doOnError(e => Task(log.error("Got an error in the combined stream", e)))

  // TODO DEX-1013
  private def processUtxEvents(utxUpdate: UtxUpdate): Set[Address] =
    if (utxUpdate.resetCaches) pessimisticPortfolios.replaceWith(utxUpdate.unconfirmedTxs)
    else
      pessimisticPortfolios.addPending(utxUpdate.unconfirmedTxs) |+|
      pessimisticPortfolios.processConfirmed(utxUpdate.confirmedTxs.keySet)._1 |+|
      pessimisticPortfolios.removeFailed(utxUpdate.failedTxs.keySet)

  // TODO DEX-1015
  private def requestBalances(x: DiffIndex): Unit =
    if (!x.isEmpty)
      meClient.getBalances(x).onComplete {
        case Success(r) => dataUpdates.onNext(WavesNodeEvent.DataReceived(r))
        case Failure(e) =>
          log.warn("Got an error during requesting balances", e)
          requestBalances(x)
      }

  private def isExchangeTxFromMatcher(tx: SignedTransaction): Boolean =
    tx.transaction.exists { tx =>
      tx.data.isExchange && ByteString.unsignedLexicographicalComparator().compare(tx.senderPublicKey, pbMatcherPublicKey) == 0
    }

  private def isExchangeTxFromMatcher(tx: UtxTransaction): Boolean = tx.transaction.exists(isExchangeTxFromMatcher)

  override def partialBalancesSnapshot(address: Address, assets: Set[Asset]): Future[AddressBalanceUpdates] =
    (
      meClient.getAddressPartialRegularBalance(address, assets),
      if (assets.contains(Asset.Waves)) meClient.getOutgoingLeasing(address).map(_.some) else Future.successful(none)
    ).mapN { case (regular, outgoingLeasing) =>
      def pred(p: (Asset, Long)): Boolean = assets.contains(p._1)
      val full = getFreshBalances(address, regular, outgoingLeasing)
      full.copy(
        regular = full.regular.filter(pred),
        pessimisticCorrection = full.pessimisticCorrection.filter(pred)
      )
    }

  override def fullBalancesSnapshot(address: Address, excludeAssets: Set[Asset]): Future[AddressBalanceUpdates] =
    (
      meClient.getAddressFullRegularBalance(address, excludeAssets),
      if (excludeAssets.contains(Asset.Waves)) Future.successful(none[Long]) else meClient.getOutgoingLeasing(address).map(_.some)
    ).mapN { case (regular, outgoingLeasing) =>
      def pred(p: (Asset, Long)): Boolean = !excludeAssets.contains(p._1)
      val full = getFreshBalances(address, regular, outgoingLeasing)
      full.copy(
        regular = full.regular.filter(pred),
        pessimisticCorrection = full.pessimisticCorrection.filter(pred)
      )
    }

  private def getFreshBalances(address: Address, regular: Map[Asset, Long], outgoingLeasing: Option[Long]): AddressBalanceUpdates = {
    val prioritizedLastUpdates = lastUpdates :+ mkBlockchainBalance(address, regular, outgoingLeasing)
    val r = prioritizedLastUpdates.map(_.regular.getOrElse(address, Map.empty))
    AddressBalanceUpdates(
      regular = r.foldSkipped,
      outgoingLeasing =
        if (outgoingLeasing.isEmpty) none[Long]
        else prioritizedLastUpdates.map(_.outgoingLeasing.get(address)).foldLeft(none[Long])(_.orElse(_)),
      pessimisticCorrection = pessimisticPortfolios.getAggregated(address)
    )
  }

  private def mkBlockchainBalance(address: Address, regular: Map[Asset, Long], outgoingLeasing: Option[Long]): BlockchainBalance =
    BlockchainBalance(
      regular = Map(address -> regular),
      outgoingLeasing = outgoingLeasing.fold(Map.empty[Address, Long])(x => Map(address -> x))
    )

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

  override def broadcastTx(tx: ExchangeTransaction): Future[BroadcastResult] = meClient.broadcastTx(tx)
  override def checkedBroadcastTx(tx: ExchangeTransaction): Future[CheckedBroadcastResult] = meClient.checkedBroadcastTx(tx)

  override def isOrderConfirmed(orderId: ByteStr): Future[Boolean] =
    meClient.isOrderConfirmed(orderId)

  override def close(): Future[Unit] =
    meClient.close().zip(bClient.close()).map(_ => ())

}

object CombinedWavesBlockchainClient extends ScorexLogging {

  case class Settings(
    maxRollbackHeight: Int,
    maxCachedLatestBlockUpdates: Int,
    combinedStream: CombinedStream.Settings,
    pessimisticPortfolios: SynchronizedPessimisticPortfolios.Settings
  )

  // TODO DEX-998
  def apply(
    wavesBlockchainClientSettings: WavesBlockchainClientSettings,
    matcherPublicKey: PublicKey,
    monixScheduler: Scheduler,
    grpcExecutionContext: ExecutionContext
  ): WavesBlockchainClient = {

    val eventLoopGroup = new NioEventLoopGroup

    log.info(s"Building Matcher Extension gRPC client for server: ${wavesBlockchainClientSettings.grpc.target}")
    val matcherExtensionChannel =
      wavesBlockchainClientSettings.grpc.toNettyChannelBuilder
        .executor((command: Runnable) => grpcExecutionContext.execute(command))
        .eventLoopGroup(eventLoopGroup)
        .channelType(classOf[NioSocketChannel])
        .usePlaintext()
        .build

    log.info(s"Building Blockchain Updates Extension gRPC client for server: ${wavesBlockchainClientSettings.blockchainUpdatesGrpc.target}")
    val blockchainUpdatesChannel =
      new RestartableManagedChannel(() =>
        wavesBlockchainClientSettings.blockchainUpdatesGrpc.toNettyChannelBuilder
          .executor((command: Runnable) => grpcExecutionContext.execute(command))
          .eventLoopGroup(eventLoopGroup)
          .channelType(classOf[NioSocketChannel])
          .usePlaintext()
          .build
      )

    new CombinedWavesBlockchainClient(
      wavesBlockchainClientSettings.combinedClientSettings,
      matcherPublicKey,
      meClient = new MatcherExtensionCachingClient(
        new MatcherExtensionGrpcAsyncClient(eventLoopGroup, matcherExtensionChannel, monixScheduler)(grpcExecutionContext),
        wavesBlockchainClientSettings.defaultCachesExpiration
      )(grpcExecutionContext),
      bClient = new DefaultBlockchainUpdatesClient(
        eventLoopGroup,
        blockchainUpdatesChannel,
        monixScheduler,
        wavesBlockchainClientSettings.blockchainUpdatesGrpc.noDataTimeout
      )(grpcExecutionContext)
    )(grpcExecutionContext, monixScheduler)
  }

}
