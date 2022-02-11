package com.wavesplatform.dex.grpc.integration.services

import cats.implicits.catsSyntaxOptionId
import cats.syntax.either._
import cats.syntax.option._
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.grpc._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.collections.Implicits.ListOps
import com.wavesplatform.dex.grpc.integration._
import com.wavesplatform.dex.grpc.integration.error.canRetry
import com.wavesplatform.dex.grpc.integration.protobuf.EitherVEExt
import com.wavesplatform.dex.grpc.integration.protobuf.PbToWavesConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions._
import com.wavesplatform.dex.grpc.integration.smart.MatcherScriptRunner
import com.wavesplatform.dex.grpc.integration.smart.MatcherScriptRunner.deniedBlockchain
import com.wavesplatform.dex.grpc.integration.utx.UtxState
import com.wavesplatform.extensions.{Context => ExtensionContext}
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.smart.script.ScriptRunnerFixed
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.{ServerCallStreamObserver, StreamObserver}
import io.grpc.{Metadata, Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import shapeless.Coproduct

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import com.wavesplatform.state.TxMeta

class WavesBlockchainApiGrpcService(context: ExtensionContext, allowedBlockchainStateAccounts: Set[ByteStr])(implicit sc: Scheduler)
    extends WavesBlockchainApiGrpc.WavesBlockchainApi
    with ScorexLogging {

  private val descKey = Metadata.Key.of("desc", Metadata.ASCII_STRING_MARSHALLER)

  private val utxState = Atomic(UtxState(accounts = allowedBlockchainStateAccounts.map { publicKey =>
    PublicKey(publicKey).toAddress(context.settings.blockchainSettings.addressSchemeCharacter.toByte)
  }))

  private val empty = Empty()

  private val utxChangesSubscribers = ConcurrentHashMap.newKeySet[StreamObserver[UtxEvent]](2)

  private val initialEvents = ConcurrentSubject.publish[(StreamObserver[UtxEvent], UtxEvent)]

  private val cleanupTask: Task[Unit] = Task {
    log.info("Closing balance changes stream...")

    // https://github.com/grpc/grpc/blob/master/doc/statuscodes.md
    val metadata = new Metadata()
    metadata.put(descKey, "Shutting down")
    val shutdownError = new StatusRuntimeException(Status.UNAVAILABLE, metadata) // Because it should try to connect to other DEX Extension

    withUtxChangesSubscribers("send onError", _.onError(shutdownError))
    utxChangesSubscribers.clear()
  }

  private val utxEvents = Observable(
    initialEvents.map(_.asLeft[com.wavesplatform.events.UtxEvent]),
    context.utxEvents.map(_.asRight[(StreamObserver[UtxEvent], UtxEvent)])
  ).merge
    .doOnSubscriptionCancel(cleanupTask)
    .doOnComplete(cleanupTask)
    .doOnError(e => Task(log.error("Error in real time balance changes stream occurred!", e)))
    .foreach {
      case Left((observer, evt)) => observer.onNext(evt) // See getUtxEvents

      case Right(evt) =>
        utxState
          .transformAndExtract(_.handleEvent(evt))
          .foreach(event => withUtxChangesSubscribers("send next", _.onNext(event)))
    }

  override def getStatuses(request: TransactionsByIdRequest): Future[TransactionsStatusesResponse] = Future {
    val statuses = request.transactionIds.map { txId =>
      context.blockchain.transactionInfo(txId.toVanilla).map(_._1) match {
        case Some(TxMeta(height, _, _)) => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, height)
        case None =>
          context.utx.transactionById(txId.toVanilla) match {
            case Some(_) => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED)
            case None => TransactionStatus(txId, TransactionStatus.Status.NOT_EXISTS)
          }
      }
    }
    TransactionsStatusesResponse(statuses)
  }

  override def broadcast(request: BroadcastRequest): Future[BroadcastResponse] = Future {
    request.transaction
      .fold[Either[ValidationError, SignedExchangeTransaction]](GenericError("The signed transaction must be specified").asLeft)(_.asRight)
      .flatMap(_.toVanilla)
      .flatMap { tx =>
        context.broadcastTransaction(tx).resultE
          .map(isNew => BroadcastResponse(BroadcastResponse.Result.Added(isNew)))
          .leftFlatMap(e => BroadcastResponse(BroadcastResponse.Result.Failed(e.toString)).asRight)
      }
      .explicitGetErr()
  }

  override def checkedBroadcast(request: CheckedBroadcastRequest): Future[CheckedBroadcastResponse] =
    Future {
      for {
        grpcTx <- request.transaction
          .fold(GenericError("The signed transaction must be specified").asLeft[SignedExchangeTransaction])(_.asRight[GenericError])
        tx <- grpcTx.toVanilla
        isConfirmed <- context.transactionsApi.transactionById(tx.id()).fold(false)(_ => true).asRight
        isInUtx <- context.transactionsApi.unconfirmedTransactionById(tx.id()).fold(false)(_ => true).asRight
      } yield (tx, isConfirmed, isInUtx)
    }
      .flatMap {
        case Right((tx, isConfirmed, isInUtx)) =>
          if (isConfirmed) Future.successful(CheckedBroadcastResponse.Result.Confirmed(empty))
          else if (isInUtx) handleTxInUtx(tx)
          else broadcastTransaction(tx).map {
            _.resultE match {
              case Right(isNew) => CheckedBroadcastResponse.Result.Unconfirmed(isNew)
              case Left(e) => CheckedBroadcastResponse.Result.Failed(CheckedBroadcastResponse.Failure(e.toString, canRetry(e)))
            }
          }
        case Left(e) => Future.successful(CheckedBroadcastResponse.Result.Failed(CheckedBroadcastResponse.Failure(e.toString, canRetry(e))))
      }
      .map(CheckedBroadcastResponse(_))
      .recover {
        case e: Throwable =>
          log.error("Broadcast failed", e)
          val message = Option(e.getMessage).getOrElse(e.getClass.getName)
          CheckedBroadcastResponse(CheckedBroadcastResponse.Result.Failed(CheckedBroadcastResponse.Failure(message)))
      }

  private def handleTxInUtx(tx: exchange.ExchangeTransaction): Future[CheckedBroadcastResponse.Result] =
    broadcastTransaction(tx).map {
      _.resultE match {
        case Right(_) => CheckedBroadcastResponse.Result.Unconfirmed(false)
        case Left(e) =>
          log.error(s"Error rebroadcasting transaction: $e")
          CheckedBroadcastResponse.Result.Unconfirmed(false)
      }
    }

  private def broadcastTransaction(tx: exchange.ExchangeTransaction): Future[TracedResult[ValidationError, Boolean]] =
    context.transactionsApi.broadcastTransaction(tx).andThen {
      case Success(r) => log.info(s"Broadcast ${tx.id()}: ${r.resultE}")
      case Failure(e) => log.warn(s"Can't broadcast ${tx.id()}", e)
    }

  override def isFeatureActivated(request: IsFeatureActivatedRequest): Future[IsFeatureActivatedResponse] = Future {
    IsFeatureActivatedResponse(
      context.blockchain.featureStatus(request.featureId.toShort, context.blockchain.height) == BlockchainFeatureStatus.Activated
    )
  }

  override def assetDescription(request: AssetIdRequest): Future[AssetDescriptionResponse] = Future {
    import AssetDescriptionResponse._

    val desc = context.blockchain.assetDescription(IssuedAsset(request.assetId.toVanilla))
    val gRpcDesc = desc.fold[MaybeDescription](MaybeDescription.Empty) { desc =>
      MaybeDescription.Description(
        AssetDescription(
          name = desc.name,
          decimals = desc.decimals,
          hasScript = desc.script.nonEmpty,
          nft = desc.nft
        )
      )
    }

    AssetDescriptionResponse(gRpcDesc)
  }

  override def hasAssetScript(request: AssetIdRequest): Future[HasScriptResponse] = Future {
    HasScriptResponse(has = context.blockchain.hasAssetScript(IssuedAsset(request.assetId.toVanilla)))
  }

  override def runAssetScript(request: RunAssetScriptRequest): Future[RunScriptResponse] = Future {
    import RunScriptResponse._

    val asset = IssuedAsset(request.assetId.toVanilla)
    val r = context.blockchain.assetScript(asset) match {
      case None => Result.Empty
      case Some(info) =>
        val tx = request.transaction
          .getOrElse(throwInvalidArgument("Expected a transaction"))
          .toVanilla
          .getOrElse(throwInvalidArgument("Can't parse the transaction"))

        parseScriptResult(
          ScriptRunnerFixed(
            in = Coproduct(tx),
            blockchain = CompositeBlockchain(context.blockchain, utxState.get().getAccountsDiff(context.blockchain)),
            script = info.script,
            isAssetScript = true,
            scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(asset.byteRepr))
          )._3
        )
    }
    RunScriptResponse(r)
  }

  override def hasAddressScript(request: HasAddressScriptRequest): Future[HasScriptResponse] = Future {
    Address
      .fromBytes(request.address.toVanilla.arr)
      .map { addr =>
        HasScriptResponse(has = context.blockchain.hasAccountScript(addr))
      }
      .explicitGetErr()
  }

  override def runAddressScript(request: RunAddressScriptRequest): Future[RunScriptResponse] = Future {
    import RunScriptResponse._

    val address = Address.fromBytes(request.address.toVanilla.arr).explicitGetErr()
    val r = context.blockchain.accountScript(address) match {
      case None => Result.Empty
      case Some(scriptInfo) =>
        val order = request.order.map(_.toVanilla).getOrElse(throwInvalidArgument("Expected an order"))
        val isSynchronousCallsActivated = context.blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)
        val useNewPowPrecision = context.blockchain.height > context.blockchain.settings.functionalitySettings.syncDAppCheckPaymentsHeight
        val correctFunctionCallScope =
          context.blockchain.height > context.blockchain.settings.functionalitySettings.estimatorSumOverflowFixHeight
        if (allowedBlockchainStateAccounts.contains(order.senderPublicKey)) {
          val blockchain = CompositeBlockchain(context.blockchain, utxState.get().getAccountsDiff(context.blockchain))
          parseScriptResult(MatcherScriptRunner(
            scriptInfo.script,
            order,
            blockchain,
            isSynchronousCallsActivated,
            useNewPowPrecision,
            correctFunctionCallScope
          ))
        } else
          parseScriptResult(MatcherScriptRunner(
            scriptInfo.script,
            order,
            deniedBlockchain,
            isSynchronousCallsActivated,
            useNewPowPrecision,
            correctFunctionCallScope
          ))
    }

    RunScriptResponse(r)
  }

  override def getOutgoingLeasing(request: AddressRequest): Future[GetOutgoingLeasingResponse] = Future {
    val address = request.address.toVanillaAddress
    GetOutgoingLeasingResponse(context.blockchain.leaseBalance(address).out)
  }

  override def getAddressPartialRegularBalance(request: GetAddressPartialRegularBalanceRequest)
    : Future[GetAddressPartialRegularBalanceResponse] = Future {
    val address = request.address.toVanillaAddress

    val assetsBalances =
      request.assetIds
        .map { requestedAssetRecord =>
          GetAddressPartialRegularBalanceResponse.Record(
            requestedAssetRecord.assetId,
            context.blockchain.balance(address, requestedAssetRecord.assetId.toVanillaAsset)
          )
        }
        .filter(_.balance > 0)

    GetAddressPartialRegularBalanceResponse(assetsBalances)
  }

  override def getAddressFullRegularBalance(request: GetAddressFullRegularBalanceRequest): Future[GetAddressFullRegularBalanceResponse] = {
    // TODO DEX-997 optimize
    for {
      address <- Task.fromTry(Try(request.address.toVanillaAddress))
      assetBalances <- context.accountsApi.portfolio(address).toListL
      excludeAssets = request.excludeAssetIds.view.map(_.toVanillaAsset).toSet
      wavesBalance <- if (excludeAssets.contains(Waves)) Task.now(none) else Task(context.accountsApi.balance(address).some)
    } yield GetAddressFullRegularBalanceResponse(
      (assetBalances: List[(Asset, Long)])
        .view
        .filterNot { case (asset, v) => excludeAssets.contains(asset) || v == 0 }
        .map { case (asset, v) => GetAddressFullRegularBalanceResponse.Record(asset.toPB, v) }
        .toList
        .prependIf(wavesBalance.nonEmpty) {
          GetAddressFullRegularBalanceResponse.Record(Waves.toPB, wavesBalance.get)
        }
    )
  }.runToFuture

  // TODO DEX-997 optimize
  override def getBalances(request: GetBalancesRequest): Future[GetBalancesResponse] = Future {
    val regular = request.regular
      .map { regular =>
        val address = regular.address.toVanillaAddress
        GetBalancesResponse.RegularPair(
          address = regular.address,
          amount = regular.assets
            .map { asset =>
              val balance = asset.toVanillaAsset match {
                case Waves => context.accountsApi.balance(address)
                case asset: IssuedAsset => context.accountsApi.assetBalance(address, asset)
              }
              Amount(asset, balance)
            }
        )
      }

    val outgoingLeasing = request.outgoingLeasingAddresses.map { address =>
      GetBalancesResponse.OutgoingLeasingPair(
        address = address,
        amount = context.blockchain.leaseBalance(address.toVanillaAddress).out
      )
    }

    GetBalancesResponse(regular, outgoingLeasing)
  }

  override def forgedOrder(request: ForgedOrderRequest): Future[ForgedOrderResponse] = Future {
    val seen = context.blockchain.filledVolumeAndFee(request.orderId.toVanilla).volume > 0
    ForgedOrderResponse(isForged = seen)
  }

  override def getUtxEvents(request: Empty, responseObserver: StreamObserver[UtxEvent]): Unit =
    if (!utxEvents.isCompleted) responseObserver match {
      case x: ServerCallStreamObserver[_] =>
        x.setOnReadyHandler { () =>
          // We have such order of calls, because we have to guarantee non-concurrent calls to onNext
          // See StreamObserver for more details
          // Yeah, we can receive onReady multiple times!
          if (!utxChangesSubscribers.contains(responseObserver)) {
            initialEvents.onNext(responseObserver -> UtxEvent(
              UtxEvent.Type.Switch(
                UtxEvent.Switch(
                  utxState.get().getUtxTransactions
                )
              )
            ))
            utxChangesSubscribers.add(responseObserver)
            log.info(s"Registered a new utx events observer: ${x.hashCode()}")
          }
        }

        x.setOnCancelHandler { () =>
          utxChangesSubscribers.remove(x)
          log.info(s"Removed an utx events observer: ${x.hashCode()}")
        }

      case x => log.warn(s"Can't register cancel handler for $x")
    }

  override def getCurrentBlockInfo(request: Empty): Future[CurrentBlockInfoResponse] = Future {
    // ByteStr.empty is a genesis block
    val id = context.blockchain.lastBlockId.getOrElse(ByteStr.empty)
    CurrentBlockInfoResponse(
      height = context.blockchain.heightOf(id).getOrElse(0),
      blockId = id.toPB
    )
  }

  private def parseScriptResult(raw: => Either[ExecutionError, Terms.EVALUATED]): RunScriptResponse.Result = {
    import RunScriptResponse.Result
    try raw match {
      case Left(execError) => Result.ScriptError(execError)
      case Right(FALSE) => Result.Denied(Empty())
      case Right(TRUE) => Result.Empty
      case Right(x) => Result.UnexpectedResult(x.toString)
    } catch {
      case NonFatal(e) =>
        log.trace(error.formatStackTrace(e))
        Result.Exception(Exception(e.getClass.getCanonicalName, Option(e.getMessage).getOrElse("No message")))
    }
  }

  private def throwInvalidArgument(description: String): Nothing = {
    val metadata = new Metadata()
    metadata.put(descKey, description)
    throw new StatusRuntimeException(Status.INVALID_ARGUMENT, metadata)
  }

  private def withUtxChangesSubscribers(label: String, f: StreamObserver[UtxEvent] => Unit): Unit =
    utxChangesSubscribers.forEach { subscriber =>
      try f(subscriber)
      catch { case e: Throwable => log.warn(s"$subscriber: can't $label", e) }
    }

}
