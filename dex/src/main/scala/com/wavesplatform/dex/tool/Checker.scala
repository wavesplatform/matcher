package com.wavesplatform.dex.tool

import cats.instances.either._
import cats.instances.list.catsStdInstancesForList
import cats.syntax.either._
import cats.syntax.traverse._
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.{Denormalization, Normalization}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV3}
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.dex.tool.connectors.DexExtensionGrpcConnector.DetailedBalance
import com.wavesplatform.dex.tool.connectors.RestConnector._
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

  type CheckResult[A]       = Either[String, A]
  type CheckLoggedResult[A] = CheckResult[(A, String)]

  private def denormalize(value: Long, decimals: Int = testAssetDecimals.toInt): Double =
    Denormalization.denormalizeAmountAndFee(value, decimals).toDouble

  private def denormalizeWavesBalance(value: Long): Double = denormalize(value, 8)

  private def printOrder(amountAssetName: String, priceAssetName: String)(order: Order): String =
    s"${order.orderType} ${denormalize(order.amount)} $amountAssetName @ ${denormalize(order.price)} $priceAssetName, id = ${order.id()}"

  private def getAmountAndPriceAssetsInfo(f: AssetInfo, s: AssetInfo): (AssetInfo, AssetInfo) =
    if (f.asset.compatId < s.asset.compatId) s -> f else f -> s

  private def check[A](name: String)(f: => CheckResult[A]): CheckResult[A] = {
    print(s" $name... " + " " * (checkLeftIndent - name.length)); val res = f; println(res fold (identity, _ => "Passed")); res
  }

  private def checkVersion(checkName: String)(version: String): CheckResult[Unit] = check(checkName) {
    dexRest.swaggerRequest.flatMap { response =>
      val parsedVersion = (response \ "info" \ "version").get.as[String]
      Either.cond(parsedVersion == version, (), s"""Failed! Expected "$version", but got "$parsedVersion"""")
    }
  }

  private def issueAsset(name: String, description: String, quantity: Long): CheckLoggedResult[AssetInfo] = {
    val matcher: PrivateKeyAccount = PrivateKeyAccount.fromPrivateKey(env.matcherKeyPair.privateKey.base58, env.chainId)
    val tx: IssueTransactionV2     = makeIssueTx(matcher, env.chainId, name, description, quantity, testAssetDecimals, false, null, issueTxFee)
    val asset: IssuedAsset         = IssuedAsset(ByteStr(tx.getId.getBytes))
    for {
      _ <- nodeRest.broadcastTx(tx).leftMap(ex => s"Cannot broadcast transaction! $ex")
      _ <- nodeRest.repeatRequest(nodeRest getTxInfo tx)(_.isRight)
    } yield AssetInfo(asset, name) -> s"Issued ${denormalize(quantity)} $name"
  }

  private def checkBalance(checkName: String): CheckLoggedResult[DetailedBalance] = check(checkName) {

    val balance      = dexExtensionGrpc.matcherBalanceSync
    val wavesBalance = denormalizeWavesBalance(balance.get(Waves).map(_._2) getOrElse 0)

    Either.cond(
      balance.get(Waves).exists(_._2 > minMatcherValidBalance),
      balance -> balance.values.map { case (d, b) => s"${denormalize(b)} ${d.name}" }.mkString(", "),
      s"Matcher Waves balance ($wavesBalance) less than ${denormalizeWavesBalance(minMatcherValidBalance)}) Waves!"
    )
  }

  private def checkTestAsset(checkName: String)(matcherBalance: DetailedBalance, assetName: String): CheckLoggedResult[AssetInfo] =
    check(checkName) {
      matcherBalance
        .find(_._2._1.name == assetName)
        .fold { issueAsset(assetName, testAssetDescription, mnogo.coin) } {
          case (a, (d, b)) => (AssetInfo(a, d.name) -> s"Balance = ${denormalize(b)} ${d.name} (${a.toString})").asRight
        }
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

  private def checkPlacement(checkName: String)(firstAssetInfo: AssetInfo, secondAssetInfo: AssetInfo): CheckLoggedResult[Order] =
    check(checkName) {

      val (amountAssetInfo, priceAssetInfo) = getAmountAndPriceAssetsInfo(firstAssetInfo, secondAssetInfo)
      val orderType                         = if (Random.nextBoolean) BUY else SELL
      val order                             = mkMatcherOrder(AssetPair(amountAssetInfo.asset, priceAssetInfo.asset), orderType)

      for {
        _ <- dexRest.placeOrder(order)
        _ <- dexRest.waitForOrderStatus(order, OrderStatus.Accepted.name)
      } yield order -> s"Placed order ${printOrder(amountAssetInfo.name, priceAssetInfo.name)(order)}"
    }

  private def checkCancellation(checkName: String)(order: Order): CheckLoggedResult[Order] = check(checkName) {
    for {
      _ <- dexRest.cancelOrder(order, env.matcherKeyPair)
      _ <- dexRest.waitForOrderStatus(order, OrderStatus.Cancelled.name)
    } yield order -> s"Order with id ${order.id()} cancelled"
  }

  private def checkMatching(checkName: String)(firstAssetInfo: AssetInfo, secondAssetInfo: AssetInfo): CheckResult[String] = check(checkName) {

    val (amountAssetInfo, priceAssetInfo) = getAmountAndPriceAssetsInfo(firstAssetInfo, secondAssetInfo)
    val assetPair                         = AssetPair(amountAssetInfo.asset, priceAssetInfo.asset)

    val counter     = mkMatcherOrder(assetPair, BUY)
    val submitted   = mkMatcherOrder(assetPair, SELL)
    val submittedId = submitted.id()

    def checkFillingAtDex(orderStatus: JsValue): ErrorOr[Boolean] = {
      lazy val expectedFilledStatus = OrderStatus.Filled(submitted.amount, submitted.matcherFee).json.toString
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
          .repeatRequest(dexRest getTxsByOrderId submittedId)(_.isRight)
          .ensure(s"Failed! Cannot find transactions for order id $submittedId")(_.lengthCompare(1) >= 0)
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
      val printOrder: Order => String = this.printOrder(amountAssetInfo.name, priceAssetInfo.name)(_)
      s"""
         |    Counter   = ${printOrder(counter)}, status = ${counterStatus.toString}
         |    Submitted = ${printOrder(submitted)}, status = ${submittedStatus.toString}
         |    Tx ids    = ${txs.map(tx => (tx \ "id").as[String]).mkString(", ")}""".stripMargin
    }
  }

  def checkState(version: String): Unit = {
    println(s"Checking")

    val checkResult =
      for {
        _                              <- checkVersion("1. DEX version")(version)
        (balance, balanceNotes)        <- checkBalance("2. Matcher balance")
        (wuJIoInfo, firstAssetNotes)   <- checkTestAsset("3. First test asset")(balance, firstTestAssetName)
        (mbIJIoInfo, secondAssetNotes) <- checkTestAsset("4. Second test asset")(balance, secondTestAssetName)
        (order, placementNotes)        <- checkPlacement("5. Order placement")(wuJIoInfo, mbIJIoInfo)
        (_, cancellationNotes)         <- checkCancellation("6. Order cancellation")(order)
        matchingNotes                  <- checkMatching("7. Matching")(wuJIoInfo, mbIJIoInfo)
      } yield {
        s"""
           |Diagnostic notes:
           |  Matcher balance : $balanceNotes 
           |  First asset     : $firstAssetNotes
           |  Second asset    : $secondAssetNotes
           |  Placement       : $placementNotes 
           |  Cancellation    : $cancellationNotes
           |  Matching        : $matchingNotes
       """.stripMargin
      }

    checkResult match {
      case Right(notes) => println(s"\n$notes\nCongratulations! All checks passed!")
      case Left(_)      => println(s"\nChecking failed!")
    }
  }
}

object Checker {

  private case class AssetInfo(asset: Asset, name: String)

  private val firstTestAssetName  = "IIIuJIo"
  private val secondTestAssetName = "MbIJIo"

  private val testAssetDescription  = "Asset for the Matcher checking purposes"
  private val testAssetDecimals     = 8.toByte
  private val testAmount, testPrice = 1.coin
  private val mnogo                 = 100000000L

  private val checkLeftIndent = 40
  private val issueTxFee      = 1.waves
  private val matcherOrderFee = 0.003.waves

  private val minMatcherValidBalance = 3.waves

  private[tool] implicit class DoubleOps(private val value: Double) {
    val coin, waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}
