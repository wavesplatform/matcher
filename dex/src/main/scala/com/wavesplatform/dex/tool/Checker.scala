package com.wavesplatform.dex.tool

import cats.syntax.either._
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.{Denormalization, Normalization}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderV3}
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.dex.tool.connectors.DexExtensionGrpcConnector.DetailedBalance
import com.wavesplatform.dex.tool.connectors.RestConnector._
import com.wavesplatform.dex.tool.connectors.SuperConnector
import com.wavesplatform.wavesj.transactions.IssueTransactionV2
import com.wavesplatform.wavesj.{PrivateKeyAccount, Transactions}
import play.api.libs.json.Json

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

  private def check[A](name: String, doneOnSameLine: Boolean = true)(f: => CheckResult[A]): CheckResult[A] = {
    if (doneOnSameLine) print(s" $name... ") else println(name)
    val res = f
    val msg = res.fold(identity, { case s: String => s; case _ => "Passed" })
    println(msg)
    res
  }

  private def checkVersion(checkName: String)(version: String): CheckResult[Unit] = check(checkName) {
    dexRest.swaggerRequest.body.flatMap { body =>
      val parsedVersion = (Json.parse(body) \ "info" \ "version").get.as[String].trim
      Either.cond(parsedVersion == version, (), s"""Failed! Expected "$version", but got "$parsedVersion"""")
    }
  }

  private def issueAsset(name: String, description: String, quantity: Long): CheckLoggedResult[AssetInfo] = {
    val matcher: PrivateKeyAccount = PrivateKeyAccount.fromPrivateKey(env.matcherKeyPair.privateKey.base58, env.chainId)
    val tx: IssueTransactionV2     = Transactions.makeIssueTx(matcher, env.chainId, name, description, quantity, testAssetDecimals, false, null, 1.waves)
    val asset: IssuedAsset         = IssuedAsset(ByteStr(tx.getId.getBytes))
    for {
      _ <- { nodeRest.broadcastTx(tx).body.leftMap(ex => s"Cannot broadcast transaction! $ex") }
      _ <- { repeatRequest(10, 1.second)(nodeRest.txInfo(tx).body, _.isRight) }
    } yield AssetInfo(asset, name) -> s"Issued ${denormalize(quantity)} $name"
  }

  private def checkMatcherBalance(checkName: String): CheckLoggedResult[DetailedBalance] = check(checkName) {

    val balance      = dexExtensionGrpc.matcherBalance
    val wavesBalance = denormalize(balance.get(Waves).map(_._2).getOrElse(0), 8)

    Either.cond(
      balance.get(Waves).exists(_._2 > 3.waves),
      balance -> balance.values.map { case (d, b) => s"${denormalize(b)} ${d.name}" }.mkString(", "),
      s"Matcher Waves balance ($wavesBalance) less than 3 Waves!"
    )
  }

  private def checkTestAsset(checkName: String)(matcherBalance: DetailedBalance, assetName: String): CheckLoggedResult[AssetInfo] =
    check(checkName) {
      matcherBalance
        .find(_._2._1.name == assetName)
        .fold { issueAsset(assetName, testAssetDescription, mnogo.mbIJIo) } {
          case (a, (d, b)) => (AssetInfo(a, d.name) -> s"Balance = ${denormalize(b)} ${d.name} (${a.toString})").asRight
        }
    }

  private def isOrderStatus(response: Either[String, String], orderStatusName: String): Boolean =
    response.map(j => (Json.parse(j) \ "status").get.as[String]).exists(_ == orderStatusName)

  private def checkPlacement(checkName: String)(firstAssetInfo: AssetInfo, secondAssetInfo: AssetInfo): CheckLoggedResult[Order] =
    check(checkName) {

      import Ordered._

      val (amountAssetInfo, priceAssetInfo) = {
        if (firstAssetInfo.asset.compatId < secondAssetInfo.asset.compatId)
          secondAssetInfo   -> firstAssetInfo
        else firstAssetInfo -> secondAssetInfo
      }

      val assetPair = AssetPair(amountAssetInfo.asset, priceAssetInfo.asset)
      val orderType = if (Random.nextBoolean) BUY else SELL
      val amount    = 1.wuJIo
      val price     = 1.mbIJIo

      val timestamp = System.currentTimeMillis

      val order =
        OrderV3(
          env.matcherKeyPair,
          env.matcherKeyPair.publicKey,
          assetPair,
          orderType,
          amount,
          price,
          timestamp,
          timestamp + 24.hours.toMillis,
          0.003.waves,
          Waves
        )

      for {
        _ <- dexRest.placeOrder(order).body
        _ <- repeatRequest(10, 1.second)(dexRest.getOrderStatus(order).body, isOrderStatus(_, OrderStatus.Accepted.name))
      } yield {
        order -> s"Placed $orderType ${denormalize(amount)} ${amountAssetInfo.name} @ ${denormalize(price)} ${priceAssetInfo.name}, order id = ${order.id()}"
      }
    }

  private def checkCancellation(checkName: String)(order: Order): CheckLoggedResult[Order] = check(checkName) {
    for {
      _ <- dexRest.cancelOrder(order, env.matcherKeyPair).body
      _ <- repeatRequest(10, 1.second)(dexRest.getOrderStatus(order).body, isOrderStatus(_, OrderStatus.Cancelled.name))
    } yield order -> s"Order with id ${order.id()} cancelled"
  }

  def checkState(version: String): Unit = {
    println(s"Checking")

    val checkResult =
      for {
        _                                     <- checkVersion("1. DEX version")(version)
        (matcherBalance, matcherBalanceNotes) <- checkMatcherBalance("2. Matcher balance")
        (wuJIoInfo, firstAssetNotes)          <- checkTestAsset("3. First test asset")(matcherBalance, firstTestAssetName)
        (mbIJIoInfo, secondAssetNotes)        <- checkTestAsset("4. Second test asset")(matcherBalance, secondTestAssetName)
        (order, placementNotes)               <- checkPlacement("5. Order placement")(wuJIoInfo, mbIJIoInfo)
        (_, cancellationNotes)                <- checkCancellation("6. Order cancellation")(order)
      } yield {
        s"""
           |Diagnostic notes:
           |  Matcher balance : $matcherBalanceNotes 
           |  First asset     : $firstAssetNotes
           |  Second asset    : $secondAssetNotes
           |  Placement       : $placementNotes 
           |  Cancellation    : $cancellationNotes
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

  private val testAssetDescription = "Asset for the Matcher checking purposes"
  private val testAssetDecimals    = 8.toByte

  private val mnogo = 100000000L

  implicit class DoubleOps(private val value: Double) {
    val wuJIo, mbIJIo, waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}
