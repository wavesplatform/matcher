package com.wavesplatform.dex.grpc.integration.clients.matcherext

import com.google.protobuf.UnsafeByteOperations
import com.google.protobuf.empty.Empty
import com.wavesplatform.api.grpc.TransactionsByIdRequest
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain.{BlockRef, BlockchainBalance, DiffIndex}
import com.wavesplatform.dex.grpc.integration.clients.{BroadcastResult, CheckedBroadcastResult, RunScriptResult}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.dex.grpc.integration.exceptions.{UnexpectedConnectionException, WavesNodeConnectionLostException}
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.WavesBlockchainApiGrpc._
import com.wavesplatform.dex.grpc.integration.services._
import io.grpc._
import io.grpc.stub.{ClientCalls, StreamObserver}
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
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

  private val empty: Empty = Empty()

  private def parse(input: RunScriptResponse): RunScriptResult = {
    import RunScriptResponse.Result
    input.result match {
      case Result.WrongInput(message) => throw new IllegalArgumentException(message)
      case Result.Empty => RunScriptResult.Allowed
      case Result.ScriptError(message) => RunScriptResult.ScriptError(message)
      case Result.UnexpectedResult(obj) => RunScriptResult.UnexpectedResult(obj)
      case Result.Exception(value) => RunScriptResult.Exception(value.name, value.message)
      case _: Result.Denied => RunScriptResult.Denied
    }
  }

  override val utxEvents = new GrpcUtxEventsControlledStream(channel)(monixScheduler)

  override def getOutgoingLeasing(address: Address): Future[Long] = handlingErrors {
    asyncUnaryCall(
      METHOD_GET_OUTGOING_LEASING,
      AddressRequest(address = address.toPB)
    ).map(_.outgoingLeasing)
  }

  override def getAddressPartialRegularBalance(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = handlingErrors {
    asyncUnaryCall(
      METHOD_GET_ADDRESS_PARTIAL_REGULAR_BALANCE,
      GetAddressPartialRegularBalanceRequest(address = address.toPB, assets.map(a => GetAddressPartialRegularBalanceRequest.Record(a.toPB)).toSeq)
    ).map(response => response.balances.map(record => record.assetId.toVanillaAsset -> record.balance).toMap)
  }

  override def getAddressFullRegularBalance(address: Address, excludeAssets: Set[Asset]): Future[Map[Asset, Long]] = handlingErrors {
    asyncUnaryCall(
      METHOD_GET_ADDRESS_FULL_REGULAR_BALANCE,
      GetAddressFullRegularBalanceRequest(address.toPB, excludeAssetIds = excludeAssets.map(_.toPB).toSeq)
    ).map(response => response.balances.map(record => record.assetId.toVanillaAsset -> record.balance).toMap)
  }

  override def getBalances(index: DiffIndex): Future[BlockchainBalance] = handlingErrors {
    val request = GetBalancesRequest(
      regular = index.regular.map { case (address, assets) =>
        GetBalancesRequest.RegularPair(
          address = address.toPB,
          assets = assets.map(_.toPB).toSeq
        )
      }.toSeq,
      outgoingLeasingAddresses = index.outgoingLeasing.map(_.toPB).toSeq
    )

    asyncUnaryCall(WavesBlockchainApiGrpc.METHOD_GET_BALANCES, request)
      .map { response =>
        BlockchainBalance(
          regular = response.regular
            .map(pair => pair.address.toVanillaAddress -> pair.amount.map(x => x.assetId.toVanillaAsset -> x.amount).toMap)
            .toMap,
          outgoingLeasing = response.outgoingLeasing.map(x => x.address.toVanillaAddress -> x.amount).toMap
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

  override def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = handlingErrors {
    asyncUnaryCall(METHOD_GET_STATUSES, TransactionsByIdRequest(txIds.map(id => UnsafeByteOperations.unsafeWrap(id.arr))))
      .map(_.transactionsStatutes.map(txStatus => txStatus.id.toVanilla -> !txStatus.status.isNotExists).toMap)
  }.recover { case _ => txIds.map(_ -> false).toMap }

  override def broadcastTx(tx: transaction.ExchangeTransaction): Future[BroadcastResult] = handlingErrors {
    asyncUnaryCall(METHOD_BROADCAST, BroadcastRequest(transaction = Some(tx.toPB))).map { response =>
      import BroadcastResponse.Result
      response.result match {
        case Result.Empty => BroadcastResult.Failed("Unexpected response on client: Result.Empty")
        case Result.Added(x) => if (x) BroadcastResult.Added else BroadcastResult.NotAdded
        case Result.Failed(message) => BroadcastResult.Failed(message)
      }
    }
  }.recover { case e => BroadcastResult.Failed(s"Failed on client: ${Option(e.getMessage).getOrElse(e.getClass.getName)}") }

  override def checkedBroadcastTx(tx: transaction.ExchangeTransaction): Future[CheckedBroadcastResult] = handlingErrors {
    asyncUnaryCall(METHOD_CHECKED_BROADCAST, CheckedBroadcastRequest(transaction = Some(tx.toPB))).map { response =>
      import CheckedBroadcastResponse.Result
      response.result match {
        case Result.Empty => CheckedBroadcastResult.Failed("Unexpected response on client: Result.Empty", canRetry = false)
        case Result.Unconfirmed(isNew) => CheckedBroadcastResult.Unconfirmed(isNew)
        case Result.Confirmed(_) => CheckedBroadcastResult.Confirmed
        case Result.Failed(failure) => CheckedBroadcastResult.Failed(failure.message, failure.canRetry)
      }
    }
  }.recover { case e =>
    CheckedBroadcastResult.Failed(s"Failed on client: ${Option(e.getMessage).getOrElse(e.getClass.getName)}", canRetry = true)
  }

  override def isOrderConfirmed(orderId: ByteStr): Future[Boolean] = handlingErrors {
    asyncUnaryCall(METHOD_FORGED_ORDER, ForgedOrderRequest(orderId.toPB)).map(_.isForged)
  }

  override def currentBlockInfo: Future[BlockRef] = handlingErrors {
    asyncUnaryCall(METHOD_GET_CURRENT_BLOCK_INFO, empty).map(x => BlockRef(x.height, x.blockId.toVanilla))
  }

  override def close(): Future[Unit] = {
    shuttingDown.set(true)
    utxEvents.close()
    channel.shutdown()
    channel.awaitTermination(500, TimeUnit.MILLISECONDS)

    // TODO DEX-998
    if (eventLoopGroup.isShuttingDown) Future.successful(())
    else eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

  private def asyncUnaryCall[RequestT, ResponseT](
    methodDescriptor: MethodDescriptor[RequestT, ResponseT],
    arg: RequestT
  ): Future[ResponseT] = {
    // TODO DEX-1001
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
