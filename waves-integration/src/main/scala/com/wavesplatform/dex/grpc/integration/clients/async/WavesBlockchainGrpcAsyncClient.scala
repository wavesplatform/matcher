package com.wavesplatform.dex.grpc.integration.clients.async

import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.ToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.ToVanillaConversions._
import com.wavesplatform.dex.grpc.integration.services.RunScriptResponse.Result
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import mouse.any._

import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainGrpcAsyncClient(channel: ManagedChannel, monixScheduler: Scheduler)(implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainAsyncClient[Future]
    with ScorexLogging {

  private val balancesService   = WavesBalancesApiGrpc.stub(channel)
  private val blockchainService = WavesBlockchainApiGrpc.stub(channel)

  private val spendableBalanceChangesSubject = ConcurrentSubject.publish[SpendableBalanceChanges](monixScheduler)

  private def toVanilla(record: BalanceChangesResponse.Record): (Address, Asset, Long) = {
    (record.address.toVanillaAddress, record.asset.toVanillaAsset, record.balance)
  }

  /**
    * Grouping batch records of spendable balances changes by addresses, e.g. converts
    * {{{ Seq[(Address, Asset, Balance)] to Map[Address, Map[Asset, Balance]] }}}
    */
  private def groupByAddress(balanceChangesResponse: BalanceChangesResponse): SpendableBalanceChanges = {
    balanceChangesResponse.batch
      .map { toVanilla }
      .groupBy { case (address, _, _) => address }
      .mapValues { _.map { case (_, asset, balance) => asset -> balance }.toMap.withDefaultValue(0) }
  }

  private val balanceChangesObserver: StreamObserver[BalanceChangesResponse] =
    new StreamObserver[BalanceChangesResponse] {
      override def onCompleted(): Unit                         = log.info("Balance changes stream completed!")
      override def onNext(value: BalanceChangesResponse): Unit = groupByAddress(value) |> spendableBalanceChangesSubject.onNext
      override def onError(t: Throwable): Unit = {
        log.warn(s"Error while listening to the balance changes stream occurred: ${t.getMessage}. New RPC call will be performed")
        channel.resetConnectBackoff()
        requestBalanceChanges()
      }
    }

  private def parse(input: RunScriptResponse): RunScriptResult = input.result match {
    case Result.WrongInput(message)   => throw new IllegalArgumentException(message)
    case Result.Empty                 => RunScriptResult.Allowed
    case Result.ScriptError(message)  => RunScriptResult.ScriptError(message)
    case Result.UnexpectedResult(obj) => RunScriptResult.UnexpectedResult(obj)
    case Result.Exception(value)      => RunScriptResult.Exception(value.name, value.message)
    case _: Result.Denied             => RunScriptResult.Denied
  }

  /** Performs new gRPC call for receiving of the spendable balance changes stream */
  def requestBalanceChanges(): Unit = balancesService.getBalanceChanges(Empty(), balanceChangesObserver)

  /** Returns stream of the balance changes as a sequence of batches */
  def spendableBalanceChanges: Observable[SpendableBalanceChanges] = spendableBalanceChangesSubject

  def spendableBalance(address: Address, asset: Asset): Future[Long] = {
    blockchainService.spendableAssetBalance { SpendableAssetBalanceRequest(address = address.toPB, assetId = asset.toPB) }.map(_.balance)
  }

  def isFeatureActivated(id: Short): Future[Boolean] = {
    blockchainService.isFeatureActivated { IsFeatureActivatedRequest(id) }.map(_.isActivated)
  }

  def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = {
    blockchainService.assetDescription { AssetIdRequest(asset.toPB) }.map(_.maybeDescription.toVanilla)
  }

  def assetDescription(asset: Asset): Future[Option[BriefAssetDescription]] = {
    asset.fold { Future.successful(Option.empty[BriefAssetDescription]) }(assetDescription)
  }

  def assetDecimals(asset: Asset.IssuedAsset): Future[Option[Int]] = assetDescription(asset).map { _.map(_.decimals) }

  def assetDecimals(asset: Asset): Future[Option[Int]] = asset.fold { Future.successful(Option(8)) }(assetDecimals)

  def hasScript(asset: Asset.IssuedAsset): Future[Boolean] = {
    blockchainService.hasAssetScript { AssetIdRequest(assetId = asset.toPB) }.map(_.has)
  }

  def runScript(asset: Asset.IssuedAsset, input: exchange.ExchangeTransaction): Future[WavesBlockchainClient.RunScriptResult] = {
    blockchainService
      .runAssetScript { RunAssetScriptRequest(assetId = asset.toPB, transaction = Some(input.toPB)) }
      .map(parse)
  }

  def hasScript(address: Address): Future[Boolean] = {
    blockchainService.hasAddressScript { HasAddressScriptRequest(address = address.toPB) }.map(_.has)
  }

  def runScript(address: Address, input: Order): Future[WavesBlockchainClient.RunScriptResult] = {
    blockchainService
      .runAddressScript { RunAddressScriptRequest(address = address.toPB, order = Some(input.toPB)) }
      .map(parse)
  }

  def wasForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = {
    blockchainService
      .getStatuses { TransactionsByIdRequest(txIds.map(id => ByteString copyFrom id.arr)) }
      .map { _.transactionsStatutes.map(txStatus => txStatus.id.toVanilla -> txStatus.status.isConfirmed).toMap }
  }

  def broadcastTx(tx: exchange.ExchangeTransaction): Future[Boolean] = {
    blockchainService.broadcast { BroadcastRequest(transaction = Some(tx.toPB)) }.map(_.isValid)
  }

  def forgedOrder(orderId: ByteStr): Future[Boolean] = blockchainService.forgedOrder { ForgedOrderRequest(orderId.toPB) }.map(_.isForged)
}
