package com.wavesplatform.dex.grpc.integration.services

import java.net.InetAddress
import java.util.concurrent.ConcurrentHashMap

import cats.instances.map._
import cats.instances.set._
import cats.syntax.either._
import cats.syntax.monoid._
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration._
import com.wavesplatform.dex.grpc.integration.protobuf.EitherVEExt
import com.wavesplatform.dex.grpc.integration.protobuf.PbToWavesConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions._
import com.wavesplatform.dex.grpc.integration.smart.MatcherScriptRunner
import com.wavesplatform.events.UtxEvent.{TxAdded, TxRemoved}
import com.wavesplatform.events._
import com.wavesplatform.extensions.{Context => ExtensionContext}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.{ServerCallStreamObserver, StreamObserver}
import io.grpc.{Metadata, Status, StatusRuntimeException}
import monix.eval.{Coeval, Task}
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import shapeless.Coproduct

import scala.concurrent.Future
import scala.util.Try
import scala.util.control.NonFatal

class WavesBlockchainApiGrpcService(context: ExtensionContext, ignoredExchangeTxSenderPublicKey: Option[String])(implicit sc: Scheduler)
    extends WavesBlockchainApiGrpc.WavesBlockchainApi
    with ScorexLogging {

  private val descKey = Metadata.Key.of("desc", Metadata.ASCII_STRING_MARSHALLER)

  private val allSpendableBalances = new ConcurrentHashMap[Address, Map[Asset, Long]]()

  // TODO rename to balanceChangesSubscribers after release 2.1.2
  private val realTimeBalanceChangesSubscribers = ConcurrentHashMap.newKeySet[StreamObserver[BalanceChangesFlattenResponse]](2)

  private val cleanupTask: Task[Unit] = Task {
    log.info("Closing balance changes stream...")

    // https://github.com/grpc/grpc/blob/master/doc/statuscodes.md
    val metadata      = new Metadata(); metadata.put(descKey, "Shutting down")
    val shutdownError = new StatusRuntimeException(Status.UNAVAILABLE, metadata) // Because it should try to connect to other DEX Extension

    realTimeBalanceChangesSubscribers.forEach { _.onError(shutdownError) }
    realTimeBalanceChangesSubscribers.clear()
  }

  private def getAddressesChangedAssets(transactionStateUpdates: Seq[StateUpdate]): Map[Address, Set[Asset]] = {
    transactionStateUpdates.foldLeft(Map.empty[Address, Set[Asset]]) {
      case (result, stateUpdate) => result |+| stateUpdate.balances.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    }
  }

  private val txDiffs = new ConcurrentHashMap[Transaction, Diff]()

  private val blockchainBalanceUpdates: Observable[Map[Address, Set[Asset]]] = context.blockchainUpdated.map {
    case BlockAppended(_, _, _, _, _, transactionStateUpdates)   => getAddressesChangedAssets(transactionStateUpdates)
    case MicroBlockAppended(_, _, _, _, transactionStateUpdates) => getAddressesChangedAssets(transactionStateUpdates)
    case MicroBlockRollbackCompleted(toId, toHeight)             => Map.empty[Address, Set[Asset]]
    case RollbackCompleted(toId, toHeight)                       => Map.empty[Address, Set[Asset]]
  }

  private val utxBalanceUpdates: Observable[Map[Address, Set[Asset]]] = context.utxEvents.map {
    case TxAdded(tx, diff) =>
      tx match {
        case et: ExchangeTransaction if ignoredExchangeTxSenderPublicKey.contains(et.sender.toString) => Map.empty[Address, Set[Asset]]
        case otherTx =>
          txDiffs.putIfAbsent(otherTx, diff); diff.portfolios.view.mapValues(_.assetIds).toMap
      }
    case TxRemoved(tx, _) => Option(txDiffs remove tx).map(_.portfolios.view.mapValues(_.assetIds).toMap).getOrElse(Map.empty)
  }

  private val realTimeBalanceChanges: Coeval[CancelableFuture[Unit]] = Coeval.evalOnce {
    Observable(blockchainBalanceUpdates, utxBalanceUpdates).merge
      .map {
        _.flatMap {
          case (address, assets) =>
            val addressBalance = allSpendableBalances.getOrDefault(address, Map.empty)
            assets.map { asset =>
              val newAssetBalance = spendableBalance(address, asset)
              val needUpdate      = !addressBalance.get(asset).contains(newAssetBalance)
              if (needUpdate) {
                allSpendableBalances.put(address, addressBalance + (asset -> newAssetBalance))
                Some(BalanceChangesFlattenResponse(address.toPB, asset.toPB, newAssetBalance))
              } else Option.empty[BalanceChangesFlattenResponse]
            }
        }.collect { case Some(response) => response }
      }
      .doOnSubscriptionCancel(cleanupTask)
      .doOnComplete(cleanupTask)
      .foreach { batch =>
        if (batch.nonEmpty) {
          realTimeBalanceChangesSubscribers.forEach { subscriber =>
            try batch.foreach(subscriber.onNext)
            catch { case e: Throwable => log.warn(s"Can't send balance changes to $subscriber", e) }
          }
        }
      }
  }

  override def getStatuses(request: TransactionsByIdRequest): Future[TransactionsStatusesResponse] = Future {
    val statuses = request.transactionIds.map { txId =>
      context.blockchain.transactionInfo(txId.toVanilla).map(_._1) match {
        case Some(height) => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, height) // TODO
        case None =>
          context.utx.transactionById(txId.toVanilla) match {
            case Some(_) => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED)
            case None    => TransactionStatus(txId, TransactionStatus.Status.NOT_EXISTS)
          }
      }
    }
    TransactionsStatusesResponse(statuses)
  }

  override def broadcast(request: BroadcastRequest): Future[BroadcastResponse] = Future {
    request.transaction
      .fold[Either[ValidationError, SignedExchangeTransaction]](GenericError("The signed transaction must be specified").asLeft)(_.asRight)
      .flatMap { _.toVanilla }
      .flatMap { tx =>
        if (context.blockchain.containsTransaction(tx)) Right(BroadcastResponse(isValid = true))
        else context.broadcastTransaction(tx).resultE.map(BroadcastResponse(_)).leftFlatMap(_ => BroadcastResponse().asRight)
      }
      .explicitGetErr()
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
          hasScript = desc.script.nonEmpty
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
          ScriptRunner(
            in = Coproduct(tx),
            blockchain = context.blockchain,
            script = info.script,
            isAssetScript = true,
            scriptContainerAddress = Coproduct[Environment.Tthis](Environment.AssetId(asset.byteRepr))
          )._2)
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
        parseScriptResult(MatcherScriptRunner(scriptInfo.script, order))
    }

    RunScriptResponse(r)
  }

  override def spendableAssetsBalances(request: SpendableAssetsBalancesRequest): Future[SpendableAssetsBalancesResponse] = Future {

    val address = Address.fromBytes(request.address.toVanilla.arr).explicitGetErr()

    val assetsBalances =
      request.assetIds
        .map { requestedAssetRecord =>
          SpendableAssetsBalancesResponse.Record(
            requestedAssetRecord.assetId,
            spendableBalance(address, requestedAssetRecord.assetId.toVanillaAsset)
          )
        }
        .filter(_.balance > 0)

    SpendableAssetsBalancesResponse(assetsBalances)
  }

  override def forgedOrder(request: ForgedOrderRequest): Future[ForgedOrderResponse] = Future {
    val seen = context.blockchain.filledVolumeAndFee(request.orderId.toVanilla).volume > 0
    ForgedOrderResponse(isForged = seen)
  }

  // TODO rename to getBalanceChanges after release 2.1.2
  override def getRealTimeBalanceChanges(request: Empty, responseObserver: StreamObserver[BalanceChangesFlattenResponse]): Unit =
    if (!realTimeBalanceChanges().isCompleted) {
      responseObserver match {
        case x: ServerCallStreamObserver[_] => x.setOnCancelHandler(() => realTimeBalanceChangesSubscribers remove x)
        case x                              => log.warn(s"Can't register cancel handler for $x")
      }
      realTimeBalanceChangesSubscribers.add(responseObserver)
    }

  private def parseScriptResult(raw: => Either[ExecutionError, Terms.EVALUATED]): RunScriptResponse.Result = {
    import RunScriptResponse.Result
    try raw match {
      case Left(execError) => Result.ScriptError(execError)
      case Right(FALSE)    => Result.Denied(Empty())
      case Right(TRUE)     => Result.Empty
      case Right(x)        => Result.UnexpectedResult(x.toString)
    } catch {
      case NonFatal(e) =>
        log.trace(error.formatStackTrace(e))
        Result.Exception(Exception(e.getClass.getCanonicalName, Option(e.getMessage).getOrElse("No message")))
    }
  }

  override def allAssetsSpendableBalance(request: AddressRequest): Future[AllAssetsSpendableBalanceResponse] = {
    for {
      address       <- Task.fromTry(Try(request.address.toVanillaAddress))
      assetBalances <- context.accountsApi.portfolio(address).toListL
    } yield
      AllAssetsSpendableBalanceResponse(
        (Waves :: assetBalances.map(_._1))
          .map(a => AllAssetsSpendableBalanceResponse.Record(a.toPB, spendableBalance(address, a)))
          .filterNot(_.balance == 0L)
      )
  }.runToFuture

  override def getNodeAddress(request: Empty): Future[NodeAddressResponse] = Future {
    NodeAddressResponse(InetAddress.getLocalHost.getHostAddress)
  }

  // The negative spendable balance could happen if there are multiple transactions in UTX those spend more than available
  private def spendableBalance(address: Address, asset: Asset): Long = math.max(0L, context.utx.spendableBalance(address, asset))

  private def throwInvalidArgument(description: String): Nothing = {
    val metadata = new Metadata()
    metadata.put(descKey, description)
    throw new StatusRuntimeException(Status.INVALID_ARGUMENT, metadata)
  }
}
