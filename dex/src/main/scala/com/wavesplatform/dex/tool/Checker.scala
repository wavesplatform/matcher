package com.wavesplatform.dex.tool

import cats.instances.either._
import cats.instances.list.catsStdInstancesForList
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsOrderBookChanges}
import com.wavesplatform.dex.cli._
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.{Denormalization, Normalization}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV3}
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.dex.tool.connectors.DexExtensionGrpcConnector.DetailedBalance
import com.wavesplatform.dex.tool.connectors.SuperConnector
import com.wavesplatform.wavesj.PrivateKeyAccount
import com.wavesplatform.wavesj.Transactions._
import com.wavesplatform.wavesj.transactions.IssueTransactionV2
import play.api.libs.json.JsValue

import scala.Ordered._
import scala.concurrent.duration._
import scala.util.Random

// noinspection ScalaStyle
case class Checker(superConnector: SuperConnector) {

  import Checker._
  import superConnector._

  type CheckLoggedResult[A] = ErrorOr[(A, String)]

  private def logCheck[A](name: String)(f: => ErrorOr[A]): ErrorOr[A] = wrapByLogs(s"  $name... ", "Passed\n", checkLeftIndent.some)(f)

  private def denormalize(value: Long, decimals: Int = testAssetDecimals.toInt): Double =
    Denormalization.denormalizeAmountAndFee(value, decimals).toDouble

  private def denormalizeWavesBalance(value: Long): Double = denormalize(value, 8)

  private def printOrder(assetPairInfo: AssetPairInfo)(order: Order): String =
    s"${order.orderType} ${denormalize(order.amount)} ${assetPairInfo.amountAssetName} @ ${denormalize(order.price)} ${assetPairInfo.priceAssetName}, id = ${order.id()}"

  private def getAssetPairInfo(f: AssetInfo, s: AssetInfo): AssetPairInfo =
    if (f.asset.compatId < s.asset.compatId) AssetPairInfo(s, f) else AssetPairInfo(f, s)

  private def checkVersion(version: String): ErrorOr[Unit] = dexRest.getMatcherSettings.flatMap { response =>
    val parsedVersion = (response \ "matcherVersion").get.as[String]
    Either.cond(parsedVersion == version, (), s"""Failed! Expected "$version", but got "$parsedVersion"""")
  }

  private def issueAsset(name: String, description: String, quantity: Long): CheckLoggedResult[AssetInfo] = {
    val matcher: PrivateKeyAccount = PrivateKeyAccount.fromPrivateKey(env.matcherKeyPair.privateKey.base58, env.chainId)
    val tx: IssueTransactionV2     = makeIssueTx(matcher, env.chainId, name, description, quantity, testAssetDecimals, false, null, issueTxFee)
    val asset: IssuedAsset         = IssuedAsset(ByteStr(tx.getId.getBytes))
    for {
      _      <- nodeRest.broadcastTx(tx).leftMap(ex => s"Cannot broadcast issue transaction! $ex")
      _      <- nodeRest.repeatRequest(nodeRest getTxInfo tx)(_.isRight)
      height <- nodeRest.waitForHeightArise()
    } yield
      (
        AssetInfo(asset, name),
        s"Issued ${denormalize(quantity)} $name, issue tx id = ${tx.getId.getBase58String}, height = ${height - 1}"
      )
  }

  private def checkBalance: CheckLoggedResult[DetailedBalance] = {

    val balance      = dexExtensionGrpc.matcherBalanceSync(env.matcherKeyPair)
    val wavesBalance = denormalizeWavesBalance(balance.get(Waves).map(_._2) getOrElse 0)

    Either.cond(
      balance.get(Waves).exists(_._2 > minMatcherValidBalance),
      balance -> balance.values.map { case (d, b) => s"${denormalize(b)} ${d.name}" }.mkString(", "),
      s"Matcher Waves balance $wavesBalance is less than ${denormalizeWavesBalance(minMatcherValidBalance)} Waves!"
    )
  }

  private def checkTestAsset(matcherBalance: DetailedBalance, assetName: String): CheckLoggedResult[AssetInfo] =
    matcherBalance
      .find(_._2._1.name == assetName)
      .fold { issueAsset(assetName, testAssetDescription, mnogo) } {
        case (a, (d, b)) => (AssetInfo(a, d.name) -> s"Balance = ${denormalize(b)} ${d.name} (${a.toString})").asRight
      }

  private def mkMatcherOrder(assetPair: AssetPair, orderType: OrderType): Order = {
    val timestamp = System.currentTimeMillis
    OrderV3(env.matcherKeyPair,
            env.matcherKeyPair.publicKey,
            assetPair,
            orderType,
            testAmount,
            testPrice,
            timestamp,
            timestamp + 24.hours.toMillis,
            matcherOrderFee,
            Waves)
  }

  private def checkActiveOrders(firstAssetInfo: AssetInfo, secondAssetInfo: AssetInfo): CheckLoggedResult[AssetPairInfo] = {
    val assetPairInfo = getAssetPairInfo(firstAssetInfo, secondAssetInfo)
    val assetPair     = assetPairInfo.assetPair
    for {
      activeOrders <- dexRest.getActiveOrdersByPair(env.matcherKeyPair, assetPair).map { _.map(j => (j \ "id").as[ByteStr]).toList }
      _            <- activeOrders.traverse(id => dexRest.cancelOrder(id, assetPair, env.matcherKeyPair))
      _            <- activeOrders.traverse(id => dexRest.waitForOrderStatus(id, assetPair, OrderStatus.Cancelled.name))
    } yield {
      if (activeOrders.isEmpty) (assetPairInfo, s"Matcher didn't have any active orders on a test asset pair ${assetPair.toString}")
      else (assetPairInfo, s"Matcher had active orders on a test asset pair ${assetPair.toString}: ${activeOrders.mkString(", ")} cancelled")
    }
  }

  private def checkPlacement(assetPairInfo: AssetPairInfo): CheckLoggedResult[Order] = {
    val orderType = if (Random.nextBoolean()) BUY else SELL
    val order     = mkMatcherOrder(assetPairInfo.assetPair, orderType)
    for {
      _ <- dexRest.placeOrder(order)
      _ <- dexRest.waitForOrderStatus(order, OrderStatus.Accepted.name)
    } yield order -> s"Placed order ${printOrder(assetPairInfo)(order)}"
  }

  private def checkCancellation(order: Order): CheckLoggedResult[Order] =
    for {
      _ <- dexRest.cancelOrder(order, env.matcherKeyPair)
      _ <- dexRest.waitForOrderStatus(order, OrderStatus.Cancelled.name)
    } yield order -> s"Order with id ${order.id()} cancelled"

  private def checkExecution(assetPairInfo: AssetPairInfo): ErrorOr[String] = {

    val counter     = mkMatcherOrder(assetPairInfo.assetPair, BUY)
    val submitted   = mkMatcherOrder(assetPairInfo.assetPair, SELL)
    val submittedId = submitted.id()

    def checkFillingAtDex(orderStatus: JsValue): ErrorOr[Boolean] = {

      lazy val expectedFilledStatus = {
        val orderStatus = OrderStatus.Filled(submitted.amount, submitted.matcherFee)
        HttpOrderStatus.httpOrderStatusFormat.writes(HttpOrderStatus from orderStatus).toString
      }

      (
        for {
          filledAmount <- (orderStatus \ "filledAmount").asOpt[Long]
          filledFee    <- (orderStatus \ "filledFee").asOpt[Long]
        } yield filledAmount == submitted.amount && filledFee == submitted.matcherFee
      ).toRight[String](s"Check of submitted order filling failed! Expected $expectedFilledStatus, but got ${orderStatus.toString}")
    }

    def awaitSubmittedOrderAtNode: ErrorOr[Seq[JsValue]] =
      for {
        txs <- dexRest
          .repeatRequest(dexRest getTxsByOrderId submittedId)(_.isRight)(nodeRest.repeatRequestOptions)
          .ensure(s"Awaiting of the submitted order at Node failed! Cannot find transactions for order id $submittedId")(_.lengthCompare(1) >= 0)
        _ <- txs.toList.traverse(tx => nodeRest.repeatRequest(nodeRest getTxInfo tx)(_.isRight))
      } yield txs

    for {
      _               <- dexRest.placeOrder(counter)
      counterStatus   <- dexRest.waitForOrderStatus(counter, OrderStatus.Accepted.name)
      _               <- dexRest.placeOrder(submitted)
      submittedStatus <- dexRest.waitForOrderStatus(submitted, OrderStatus.Filled.name)
      _               <- checkFillingAtDex(submittedStatus)
      txs             <- awaitSubmittedOrderAtNode
    } yield {
      val printOrder: Order => String = this.printOrder(assetPairInfo)(_)
      s"""\n
         |    Counter   = ${printOrder(counter)}, json status = ${counterStatus.toString}
         |    Submitted = ${printOrder(submitted)}, json status = ${submittedStatus.toString}
         |    Tx ids    = ${txs.map(tx => (tx \ "id").as[String]).mkString(", ")}\n""".stripMargin
    }
  }

  private def checkWsOrderBook(assetPairInfo: AssetPairInfo): ErrorOr[String] =
    dexWs.subscribeForOrderBookUpdates(assetPairInfo.assetPair).map { snapshot =>
      s"""\n
         |    Got snapshot for ${assetPairInfo.assetPairName} pair:
         |    ${WsOrderBookChanges.wsOrderBookChangesFormat.writes(snapshot).toString}\n
         """.stripMargin
    }

  private def checkWsAccountUpdates(maybeSeed: Option[String]): ErrorOr[String] = {
    authServiceRest.fold { lift(s"Account updates check wasn't performed, since Auth Service REST API uri wasn't provided") } { as =>
      for {
        creds    <- as.getAuthCredentials(maybeSeed)
        snapshot <- dexWs.subscribeForAccountUpdates(creds)
      } yield s"""\n
           |    Got snapshot for ${creds.keyPair.publicKey.toAddress} address, seed = ${creds.seed}${maybeSeed.fold(" (randomly generated)")(_ => "")}:
           |    ${WsAddressChanges.wsAddressChangesFormat.writes(snapshot).toString}\n
         """.stripMargin
    }
  }

  def checkState(version: String, maybeAccountSeed: Option[String]): ErrorOr[String] =
    for {
      _                                  <- log[ErrorOr]("\nChecking:\n")
      _                                  <- logCheck("1. DEX version") { checkVersion(version) }
      (balance, balanceNotes)            <- logCheck("2. Matcher balance") { checkBalance }
      (wuJIoInfo, firstAssetNotes)       <- logCheck("3. First test asset") { checkTestAsset(balance, firstTestAssetName) }
      (mbIJIoInfo, secondAssetNotes)     <- logCheck("4. Second test asset") { checkTestAsset(balance, secondTestAssetName) }
      (assetPairInfo, activeOrdersNotes) <- logCheck("5. Matcher active orders") { checkActiveOrders(wuJIoInfo, mbIJIoInfo) }
      (order, placementNotes)            <- logCheck("6. Order placement") { checkPlacement(assetPairInfo) }
      (_, cancellationNotes)             <- logCheck("7. Order cancellation") { checkCancellation(order) }
      executionNotes                     <- logCheck("8. Execution") { checkExecution(assetPairInfo) }
      orderBookWsStreamNotes             <- logCheck("9. Order book WS stream") { checkWsOrderBook(assetPairInfo) }
      accountUpdatesWsStreamNotes        <- logCheck("10. Account updates WS stream") { checkWsAccountUpdates(maybeAccountSeed) }
    } yield {
      s"""
           |Diagnostic notes:
           |  Matcher balance           : $balanceNotes 
           |  First asset               : $firstAssetNotes
           |  Second asset              : $secondAssetNotes
           |  Matcher active orders     : $activeOrdersNotes
           |  Placement                 : $placementNotes
           |  Cancellation              : $cancellationNotes
           |  Execution                 : $executionNotes
           |  Order book WS stream      : $orderBookWsStreamNotes
           |  Account updates WS stream : $accountUpdatesWsStreamNotes
       """.stripMargin
    }
}

object Checker {

  private case class AssetInfo(asset: Asset, name: String)

  private case class AssetPairInfo(amountAssetInfo: AssetInfo, priceAssetInfo: AssetInfo) {
    val assetPair: AssetPair              = AssetPair(amountAssetInfo.asset, priceAssetInfo.asset)
    val (amountAssetName, priceAssetName) = amountAssetInfo.name -> priceAssetInfo.name
    val assetPairName                     = s"$amountAssetName-$priceAssetName"
  }

  private val firstTestAssetName  = "IIIuJIo"
  private val secondTestAssetName = "MbIJIo"

  private val testAssetDescription  = "Asset for the Matcher checking purposes"
  private val testAssetDecimals     = 8.toByte
  private val testAmount, testPrice = 1.coin
  private val mnogo                 = 100000000.coin

  private val checkLeftIndent = 35
  private val issueTxFee      = 1.waves
  private val matcherOrderFee = 0.003.waves

  private val minMatcherValidBalance = 3.waves

  private implicit class DoubleOps(private val value: Double) {
    val coin, waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}
