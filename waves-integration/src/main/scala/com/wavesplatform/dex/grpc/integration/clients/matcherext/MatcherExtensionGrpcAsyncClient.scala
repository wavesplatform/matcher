package com.wavesplatform.dex.grpc.integration.clients.matcherext

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import cats.implicits.catsSyntaxOptionId
import cats.syntax.option._
import com.google.protobuf.UnsafeByteOperations
import com.google.protobuf.empty.Empty
import com.wavesplatform.api.grpc.TransactionsByIdRequest
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.RunScriptResult
import com.wavesplatform.dex.grpc.integration.clients.status.{BlockRef, BlockchainBalance, DiffIndex, WavesNodeEvent}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.dex.grpc.integration.exceptions.{UnexpectedConnectionException, WavesNodeConnectionLostException}
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.RunScriptResponse.Result
import com.wavesplatform.dex.grpc.integration.services.WavesBlockchainApiGrpc._
import com.wavesplatform.dex.grpc.integration.services._
import io.grpc._
import io.grpc.stub.{ClientCallStreamObserver, ClientCalls, ClientResponseObserver, StreamObserver}
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{ExecutionContext, Future, Promise}

/**
 * @param eventLoopGroup Here, because this class takes ownership
 * @param monixScheduler Is not an implicit, because it is ExecutionContext too
 */
class MatcherExtensionGrpcAsyncClient(eventLoopGroup: EventLoopGroup, channel: ManagedChannel, monixScheduler: Scheduler)(
  implicit grpcExecutionContext: ExecutionContext
) extends MatcherExtensionClient
    with ScorexLogging {

  private def gRPCErrorsHandler(exception: Throwable): Throwable = exception match {
    case ex: io.grpc.StatusRuntimeException => WavesNodeConnectionLostException("Waves Node cannot be reached via gRPC", ex)
    case ex => UnexpectedConnectionException("Unexpected connection error", ex)
  }

  private def handlingErrors[A](f: => Future[A]): Future[A] = f.transform(identity, gRPCErrorsHandler)

  private val shuttingDown = new AtomicBoolean(false)
  private val utxEventsSubject = ConcurrentSubject.publish[WavesNodeEvent](monixScheduler)

  private val empty: Empty = Empty()

  private def requestUtxEvents(): Unit = {
    val call = channel.newCall(METHOD_GET_UTX_EVENTS, CallOptions.DEFAULT)
    ClientCalls.asyncServerStreamingCall(call, empty, new UtxEventsObserver(call))
  }

  private def parse(input: RunScriptResponse): RunScriptResult = input.result match {
    case Result.WrongInput(message) => throw new IllegalArgumentException(message)
    case Result.Empty => RunScriptResult.Allowed
    case Result.ScriptError(message) => RunScriptResult.ScriptError(message)
    case Result.UnexpectedResult(obj) => RunScriptResult.UnexpectedResult(obj)
    case Result.Exception(value) => RunScriptResult.Exception(value.name, value.message)
    case _: Result.Denied => RunScriptResult.Denied
  }

  override lazy val utxEvents: Observable[WavesNodeEvent] = {
    requestUtxEvents()
    utxEventsSubject
  }

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = handlingErrors {
    asyncUnaryCall(
      METHOD_SPENDABLE_ASSETS_BALANCES,
      SpendableAssetsBalancesRequest(address = address.toPB, assets.map(a => SpendableAssetsBalancesRequest.Record(a.toPB)).toSeq)
    ).map(response => response.balances.map(record => record.assetId.toVanillaAsset -> record.balance).toMap)
  }

  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] = handlingErrors {
    asyncUnaryCall(METHOD_ALL_ASSETS_SPENDABLE_BALANCE, AddressRequest(address.toPB))
      .map(response => response.balances.map(record => record.assetId.toVanillaAsset -> record.balance).toMap)
  }

  override def getBalances(index: DiffIndex): Future[BlockchainBalance] = handlingErrors {
    val request = GetBalancesRequest(
      regular = index.regular.map { case (address, assets) =>
        GetBalancesRequest.RegularPair(
          address = address.toPB,
          assets = assets.map(_.toPB).toSeq
        )
      }.toSeq,
      outLeaseAddresses = index.outLeases.map(_.toPB).toSeq
    )

    asyncUnaryCall(WavesBlockchainApiGrpc.METHOD_GET_BALANCES, request)
      .map { response =>
        BlockchainBalance(
          regular = response.regular
            .map(pair => pair.address.toVanillaAddress -> pair.amount.map(x => x.assetId.toVanillaAsset -> x.amount).toMap)
            .toMap,
          outLeases = response.outLeases.map(x => x.address.toVanillaAddress -> x.amount).toMap
        )
      }
  }

  override def isFeatureActivated(id: Short): Future[Boolean] = handlingErrors {
    asyncUnaryCall(METHOD_IS_FEATURE_ACTIVATED, IsFeatureActivatedRequest(id)).map(_.isActivated)
  }

  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = handlingErrors {
    asyncUnaryCall(METHOD_ASSET_DESCRIPTION, AssetIdRequest(asset.toPB)).map(_.maybeDescription.toVanilla)
  }

  override def hasScript(asset: Asset.IssuedAsset): Future[Boolean] = handlingErrors {
    asyncUnaryCall(METHOD_HAS_ASSET_SCRIPT, AssetIdRequest(asset.toPB)).map(_.has)
  }

  override def runScript(asset: Asset.IssuedAsset, input: transaction.ExchangeTransaction): Future[RunScriptResult] = handlingErrors {
    asyncUnaryCall(METHOD_RUN_ASSET_SCRIPT, RunAssetScriptRequest(assetId = asset.toPB, transaction = Some(input.toPB))).map(parse)
  }

  override def hasScript(address: Address): Future[Boolean] = handlingErrors {
    asyncUnaryCall(METHOD_HAS_ADDRESS_SCRIPT, HasAddressScriptRequest(address.toPB)).map(_.has)
  }

  override def runScript(address: Address, input: Order): Future[RunScriptResult] = handlingErrors {
    asyncUnaryCall(METHOD_RUN_ADDRESS_SCRIPT, RunAddressScriptRequest(address = address.toPB, order = Some(input.toPB))).map(parse)
  }

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = handlingErrors {
    asyncUnaryCall(METHOD_GET_STATUSES, TransactionsByIdRequest(txIds.map(id => UnsafeByteOperations.unsafeWrap(id.arr))))
      .map(_.transactionsStatutes.map(txStatus => txStatus.id.toVanilla -> txStatus.status.isConfirmed).toMap)
  }.recover { case _ => txIds.map(_ -> false).toMap }

  override def broadcastTx(tx: transaction.ExchangeTransaction): Future[Boolean] = handlingErrors {
    asyncUnaryCall(METHOD_BROADCAST, BroadcastRequest(transaction = Some(tx.toPB))).map(_.isValid)
  }.recover { case _ => false }

  override def forgedOrder(orderId: ByteStr): Future[Boolean] = handlingErrors {
    asyncUnaryCall(METHOD_FORGED_ORDER, ForgedOrderRequest(orderId.toPB)).map(_.isForged)
  }

  override def currentBlockInfo: Future[BlockRef] = handlingErrors {
    asyncUnaryCall(METHOD_GET_CURRENT_BLOCK_INFO, empty).map(x => BlockRef(x.height, x.blockId.toVanilla))
  }

  override def close(): Future[Unit] = {
    shuttingDown.set(true)
    channel.shutdown()
    channel.awaitTermination(500, TimeUnit.MILLISECONDS)
    // See NettyChannelBuilder.eventLoopGroup
    eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

  final private class UtxEventsObserver(call: ClientCall[Empty, UtxEvent]) extends ClientResponseObserver[Empty, UtxEvent] with AutoCloseable {

    private var requestStream: ClientCallStreamObserver[Empty] = _

    override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = {
      this.requestStream = requestStream
      requestStream.setOnReadyHandler(() => log.info(s"Getting utx events from ${call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)}"))
    }

    override def onNext(value: UtxEvent): Unit = toEvent(value).foreach(utxEventsSubject.onNext)

    override def onError(e: Throwable): Unit = if (!shuttingDown.get()) {
      channel.resetConnectBackoff()
      requestUtxEvents()
    }

    override def onCompleted(): Unit = log.info(s"Utx events stream completed")

    override def close(): Unit = if (requestStream != null) requestStream.cancel("Shutting down", new StatusRuntimeException(Status.CANCELLED))

    private def toEvent(event: UtxEvent): Option[WavesNodeEvent] = event.`type` match {
      case UtxEvent.Type.Switch(event) => WavesNodeEvent.UtxSwitched(event.transactions).some
      case UtxEvent.Type.Update(event) =>
        if (event.added.isEmpty) none
        else WavesNodeEvent.UtxAdded(event.added.flatMap(_.transaction)).some
      case _ =>
        log.warn(s"Can't convert to event: $event")
        none
    }

  }

  private def asyncUnaryCall[RequestT, ResponseT](
    methodDescriptor: MethodDescriptor[RequestT, ResponseT],
    arg: RequestT
  ): Future[ResponseT] = {
    // TODO better CallOptions
    val call = channel.newCall(methodDescriptor, CallOptions.DEFAULT)
    val p = Promise[ResponseT]()
    ClientCalls.asyncUnaryCall(
      call,
      arg,
      new StreamObserver[ResponseT] {
        override def onNext(value: ResponseT): Unit = p.trySuccess(value)
        override def onError(t: Throwable): Unit = p.tryFailure(t)
        override def onCompleted(): Unit = {
          val methodName = methodDescriptor.getFullMethodName.split('/').lastOption.getOrElse(methodDescriptor.getFullMethodName)
          log.trace(s"$methodName to ${call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)}")
        }
      }
    )
    p.future
  }

}
