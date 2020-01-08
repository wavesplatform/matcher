package com.wavesplatform.dex.grpc.integration.clients

import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.exceptions.{UnexpectedConnectionException, WavesNodeConnectionLostException}
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
import monix.execution.atomic.AtomicBoolean
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainGrpcAsyncClient(channel: ManagedChannel)(implicit monixScheduler: Scheduler, grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient[Future]
    with ScorexLogging {

  private def gRPCErrorsHandler(exception: Throwable): Throwable = exception match {
    case ex: io.grpc.StatusRuntimeException => WavesNodeConnectionLostException("Waves Node cannot be reached via gRPC", ex)
    case ex                                 => UnexpectedConnectionException("Unexpected connection error", ex)
  }

  private def handlingErrors[A](f: => Future[A]): Future[A] = { f transform (identity, gRPCErrorsHandler) }

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

  private val isConnectionEstablished: AtomicBoolean = AtomicBoolean(true)

  private val balanceChangesObserver: StreamObserver[BalanceChangesResponse] = new StreamObserver[BalanceChangesResponse] {

    override def onCompleted(): Unit = log.info("Balance changes stream completed!")

    override def onNext(value: BalanceChangesResponse): Unit = {
      if (isConnectionEstablished.compareAndSet(false, true)) log.info("Connection with Node restored!")
      spendableBalanceChangesSubject.onNext(groupByAddress(value))
    }

    override def onError(t: Throwable): Unit = {
      if (isConnectionEstablished.compareAndSet(true, false)) log.error("Connection with Node lost!", t)
      requestBalanceChanges()
      channel.resetConnectBackoff()
    }
  }

  /** Performs new gRPC call for receiving of the spendable balance changes stream */
  private def requestBalanceChanges(): Unit = blockchainService.getBalanceChanges(Empty(), balanceChangesObserver)

  private def parse(input: RunScriptResponse): RunScriptResult = input.result match {
    case Result.WrongInput(message)   => throw new IllegalArgumentException(message)
    case Result.Empty                 => RunScriptResult.Allowed
    case Result.ScriptError(message)  => RunScriptResult.ScriptError(message)
    case Result.UnexpectedResult(obj) => RunScriptResult.UnexpectedResult(obj)
    case Result.Exception(value)      => RunScriptResult.Exception(value.name, value.message)
    case _: Result.Denied             => RunScriptResult.Denied
  }

  /** Returns stream of the balance changes as a sequence of batches */
  override lazy val spendableBalanceChanges: Observable[SpendableBalanceChanges] = {
    requestBalanceChanges()
    spendableBalanceChangesSubject
  }

  override def spendableBalance(address: Address, asset: Asset): Future[Long] = handlingErrors {
    blockchainService
      .spendableAssetBalance { SpendableAssetBalanceRequest(address = address.toPB, assetId = asset.toPB) }
      .map(_.balance)
  }

  override def isFeatureActivated(id: Short): Future[Boolean] = handlingErrors {
    blockchainService.isFeatureActivated { IsFeatureActivatedRequest(id) }.map(_.isActivated)
  }

  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = handlingErrors {
    blockchainService.assetDescription { AssetIdRequest(asset.toPB) }.map(_.maybeDescription.toVanilla)
  }

  override def hasScript(asset: Asset.IssuedAsset): Future[Boolean] = handlingErrors {
    blockchainService.hasAssetScript { AssetIdRequest(assetId = asset.toPB) }.map(_.has)
  }

  override def runScript(asset: Asset.IssuedAsset, input: exchange.ExchangeTransaction): Future[RunScriptResult] = handlingErrors {
    blockchainService
      .runAssetScript { RunAssetScriptRequest(assetId = asset.toPB, transaction = Some(input.toPB)) }
      .map(parse)
  }

  override def hasScript(address: Address): Future[Boolean] = handlingErrors {
    blockchainService.hasAddressScript { HasAddressScriptRequest(address = address.toPB) }.map(_.has)
  }

  override def runScript(address: Address, input: Order): Future[RunScriptResult] = handlingErrors {
    blockchainService
      .runAddressScript { RunAddressScriptRequest(address = address.toPB, order = Some(input.toPB)) }
      .map(parse)
  }

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] =
    handlingErrors {
      blockchainService
        .getStatuses { TransactionsByIdRequest(txIds.map(id => ByteString copyFrom id.arr)) }
        .map { _.transactionsStatutes.map(txStatus => txStatus.id.toVanilla -> txStatus.status.isConfirmed).toMap }
    } recover { case _ => txIds.map(_ -> false).toMap }

  override def broadcastTx(tx: exchange.ExchangeTransaction): Future[Boolean] =
    handlingErrors { blockchainService.broadcast { BroadcastRequest(transaction = Some(tx.toPB)) }.map(_.isValid) } recover { case _ => false }

  override def forgedOrder(orderId: ByteStr): Future[Boolean] = handlingErrors {
    blockchainService.forgedOrder { ForgedOrderRequest(orderId.toPB) }.map(_.isForged)
  }
}
