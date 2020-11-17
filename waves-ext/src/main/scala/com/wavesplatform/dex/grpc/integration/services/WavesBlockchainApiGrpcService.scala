package com.wavesplatform.dex.grpc.integration.services

import java.net.InetAddress
import java.util.concurrent.ConcurrentHashMap

import cats.implicits.catsSyntaxOptionId
import cats.syntax.either._
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.api.grpc._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.collections.Implicits._
import com.wavesplatform.dex.grpc.integration._
import com.wavesplatform.dex.grpc.integration.protobuf.EitherVEExt
import com.wavesplatform.dex.grpc.integration.protobuf.PbToWavesConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions._
import com.wavesplatform.dex.grpc.integration.smart.MatcherScriptRunner
import com.wavesplatform.events.UtxEvent.{TxAdded, TxRemoved}
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.extensions.{Context => ExtensionContext}
import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.{ServerCallStreamObserver, StreamObserver}
import io.grpc.{Metadata, Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.Scheduler
import scalapb.json4s.JsonFormat
import shapeless.Coproduct

import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

// TODO send all utx on connect?
class WavesBlockchainApiGrpcService(context: ExtensionContext, ignoredExchangeTxSenderPublicKey: Option[String])(implicit sc: Scheduler)
    extends WavesBlockchainApiGrpc.WavesBlockchainApi
    with ScorexLogging {

  private val descKey = Metadata.Key.of("desc", Metadata.ASCII_STRING_MARSHALLER)

  private val utxState = new ConcurrentHashMap[ByteStr, UtxTransaction]()

  private val utxChangesSubscribers = ConcurrentHashMap.newKeySet[StreamObserver[UtxEvent]](2)

  private val cleanupTask: Task[Unit] = Task {
    log.info("Closing balance changes stream...")

    // https://github.com/grpc/grpc/blob/master/doc/statuscodes.md
    val metadata = new Metadata(); metadata.put(descKey, "Shutting down")
    val shutdownError = new StatusRuntimeException(Status.UNAVAILABLE, metadata) // Because it should try to connect to other DEX Extension

    utxChangesSubscribers.forEach(_.onError(shutdownError))
    utxChangesSubscribers.clear()
  }

  private val pbWaves = Waves.toPB

  case class PortfolioUpdates(
    balanceUpdates: List[StateUpdate.BalanceUpdate],
    leasingUpdates: List[StateUpdate.LeasingUpdate]
  )

  private def unpack(init: PortfolioUpdates, address: Address, portfolio: Portfolio): PortfolioUpdates = {
    val pbAddress = address.toPB

    val balanceUpdates = portfolio.assets
      .foldLeft(init.balanceUpdates) {
        case (r, (asset, v)) =>
          if (v == 0) r
          else StateUpdate.BalanceUpdate(
            address = address.toPB,
            amount = Amount(asset.toPB, v).some
          ) :: r
      }
      .prependIf(portfolio.balance != 0)(StateUpdate.BalanceUpdate(pbAddress, Amount(pbWaves, portfolio.balance).some))

    val lease = portfolio.lease
    val leasingUpdates = init.leasingUpdates.prependIf(lease.in != 0 || lease.out != 0) {
      StateUpdate.LeasingUpdate(
        address = pbAddress,
        in = portfolio.lease.in,
        out = portfolio.lease.out
      )
    }

    PortfolioUpdates(balanceUpdates, leasingUpdates)
  }

  private def toPbDiff(vanilla: Diff): TransactionDiff = {
    val portfolioUpdates = vanilla.portfolios.foldLeft(PortfolioUpdates(Nil, Nil)) {
      case (r, (address, portfolio)) => unpack(r, address, portfolio)
    }

    TransactionDiff(
      stateUpdate = StateUpdate(
        balances = portfolioUpdates.balanceUpdates,
        leases = portfolioUpdates.leasingUpdates,
        dataEntries = vanilla.accountData.view.flatMap {
          case (address, dataEntries) =>
            dataEntries.data.values.map { dataEntry =>
              StateUpdate.DataEntryUpdate(
                address = address.toPB,
                dataEntry = PBTransactions.toPBDataEntry(dataEntry).some
              )
            }
        }.toList,
        assets = Nil // vanilla. // TODO
      ).some
    )
  }

  // TODO duplication
  private def getSimpleName(x: Any): String = x.getClass.getName.replaceAll(".*?(\\w+)\\$?$", "$1")

  // TODO close
  private val utxBalanceUpdates = context.utxEvents
    .doOnSubscriptionCancel(cleanupTask)
    .doOnComplete(cleanupTask)
    .doOnError(e => Task(log.error(s"Error in real time balance changes stream occurred!", e)))
    .foreach {
      case evt @ TxAdded(tx, diff) =>
        val utxTransaction = UtxTransaction(
          id = tx.id().toPB,
          transaction = tx.toPB.some,
          diff = toPbDiff(diff).some
        )

        utxState.put(tx.id(), utxTransaction)
        log.info(s"UtxAdded: id=${tx.id()}, ${JsonFormat.toJsonString(utxTransaction)}, $evt")

        val event = UtxEvent(
          UtxEvent.Type.Update(
            UtxEvent.Update(
              added = List(UtxEvent.Update.Added(utxTransaction.some))
            )
          )
        )

        utxChangesSubscribers.forEach { subscriber =>
          try subscriber.onNext(event)
          catch { case e: Throwable => log.warn(s"Can't send balance changes to $subscriber", e) }
        }

      case evt @ TxRemoved(tx, reason) =>
        Option(utxState.remove(tx.id())) match {
          case None => log.info(s"$evt - can't find")
          case Some(utxTransaction) =>
            log.info(s"UtxRemoved: id=${tx.id()}, $evt")
            val gReason = reason.map { x =>
              UtxEvent.Update.Removed.Reason(
                name = getSimpleName(x),
                message = x.toString
              )
            }

            val event = UtxEvent(
              UtxEvent.Type.Update(
                UtxEvent.Update(
                  removed = List(UtxEvent.Update.Removed(utxTransaction.some, gReason))
                )
              )
            )

            utxChangesSubscribers.forEach { subscriber =>
              try subscriber.onNext(event)
              catch { case e: Throwable => log.warn(s"Can't send balance changes to $subscriber", e) }
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
          )._2
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

  // TODO optimize
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

    val outLeases = request.outLeaseAddresses.map { address =>
      GetBalancesResponse.OutLeasePair(
        address = address,
        amount = context.blockchain.leaseBalance(address.toVanillaAddress).out
      )
    }

    GetBalancesResponse(regular, outLeases)
  }

  override def forgedOrder(request: ForgedOrderRequest): Future[ForgedOrderResponse] = Future {
    val seen = context.blockchain.filledVolumeAndFee(request.orderId.toVanilla).volume > 0
    ForgedOrderResponse(isForged = seen)
  }

  override def getUtxEvents(request: Empty, responseObserver: StreamObserver[UtxEvent]): Unit =
    if (!utxBalanceUpdates.isCompleted) {
      responseObserver match {
        case x: ServerCallStreamObserver[_] => x.setOnCancelHandler(() => utxChangesSubscribers remove x)
        case x => log.warn(s"Can't register cancel handler for $x")
      }

      log.info("Add new observer")
      utxChangesSubscribers.add(responseObserver)
      val event = UtxEvent(
        UtxEvent.Type.Switch(
          UtxEvent.Switch(
            utxState.values().asScala.toSeq
          )
        )
      )
      responseObserver.onNext(event)
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

  override def allAssetsSpendableBalance(request: AddressRequest): Future[AllAssetsSpendableBalanceResponse] = {
    for {
      address <- Task.fromTry(Try(request.address.toVanillaAddress))
      assetBalances <- context.accountsApi.portfolio(address).toListL // TODO optimize
    } yield AllAssetsSpendableBalanceResponse(
      (Waves :: assetBalances.map(_._1))
        .map(a => AllAssetsSpendableBalanceResponse.Record(a.toPB, spendableBalance(address, a)))
        .filterNot(_.balance == 0L)
    )
  }.runToFuture

  override def getNodeAddress(request: Empty): Future[NodeAddressResponse] = Future {
    NodeAddressResponse(InetAddress.getLocalHost.getHostAddress)
  }

  private def spendableBalance(address: Address, asset: Asset): Long = {
    val stateBalance = context.blockchain.balance(address, asset)
    val leasedBalance = asset.fold(context.blockchain.leaseBalance(address).out)(_ => 0L)
    math.max(
      0L,
      stateBalance - leasedBalance // + pessimisticBalance TODO
    ) // The negative spendable balance could happen if there are multiple transactions in UTX those spend more than available
  }

  private def throwInvalidArgument(description: String): Nothing = {
    val metadata = new Metadata()
    metadata.put(descKey, description)
    throw new StatusRuntimeException(Status.INVALID_ARGUMENT, metadata)
  }

}

object WavesBlockchainApiGrpcService {}
