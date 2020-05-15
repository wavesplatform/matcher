package com.wavesplatform.dex.tool

import cats.syntax.either._
import com.wavesplatform.dex.domain.model.{Denormalization, Normalization}
import com.wavesplatform.dex.tool.connectors.{RestConnector, SuperConnector}
import com.wavesplatform.wavesj.transactions.IssueTransactionV2
import com.wavesplatform.wavesj.{PrivateKeyAccount, Transactions}
import play.api.libs.json.Json

import scala.concurrent.duration._

// noinspection ScalaStyle
case class Checker(superConnector: SuperConnector) {

  import Checker._
  import superConnector._

  type CheckResult[A] = Either[String, A]

  private def denormalize(value: Long, decimals: Int = testAssetDecimals.toInt): Double =
    Denormalization.denormalizeAmountAndFee(value, decimals).toDouble

  private def check[A](name: String, doneOnSameLine: Boolean = true)(f: => CheckResult[A]): CheckResult[A] = {
    if (doneOnSameLine) print(name) else println(name)
    val res = f
    val msg = res.fold(identity, { case () => "Done"; case s: String => s })
    println(msg)
    res
  }

  private def checkVersion(checkName: String, version: String): CheckResult[String] = check(s" $checkName ") {
    dexRest.swaggerRequest.body.flatMap { body =>
      val parsedVersion = (Json.parse(body) \ "info" \ "version").get.as[String].trim
      Either.cond(parsedVersion == version, "Passed", s"""Failed! Expected "$version", but got "$parsedVersion"""")
    }
  }

  private def issueTestAsset(name: String, description: String, quantity: Long): CheckResult[String] = {
    val matcher: PrivateKeyAccount = PrivateKeyAccount.fromPrivateKey(env.matcherKeyPair.privateKey.base58, env.chainId)
    val tx: IssueTransactionV2     = Transactions.makeIssueTx(matcher, env.chainId, name, description, quantity, testAssetDecimals, false, null, 1.waves)
    for {
      _ <- { print(s"Issuing ${denormalize(quantity)} $name... "); nodeRest.broadcastTx(tx).body.leftMap(ex => s"Cannot broadcast transaction! $ex") }
      _ <- { print(s"Awaiting tx in blockchain... "); RestConnector.repeatRequest(10, 1.second)(nodeRest.txInfo(tx).body, _.isRight) }
    } yield "Issued!"
  }

  private def checkTestAssets(checkName: String): CheckResult[String] = check(s" $checkName ", doneOnSameLine = false) {
    def findOrIssue(checkName: String, assetName: String, description: String, quantity: Long): CheckResult[String] = check(checkName) {
      dexGrpc.matcherBalance
        .find(_._2._1.name == assetName)
        .fold { print(s"not found. "); issueTestAsset(assetName, description, quantity) } {
          case (a, (d, b)) => s"found, name = ${d.name}, id = ${a.toString}, balance = ${denormalize(b)}".asRight
        }
    }

    for {
      _ <- findOrIssue("  - Amount asset ", testAmountAssetName, testAmountAssetDescription, mnogo.wuJIo)
      _ <- findOrIssue("  - Price asset ", testPriceAssetName, testPriceAssetDescription, mnogo.mbIJIo)
    } yield "    Passed"
  }

  def checkState(version: String): Unit = {
    println(s"Checking")

    val checkResult =
      for {
        _ <- checkVersion("1. DEX version...", version)
        _ <- checkTestAssets("2. Test assets existence...")
      } yield ()

    checkResult match {
      case Right(_) => println("\nCongratulations! DEX state is valid!")
      case Left(_)  => println(s"\nDEX state is INVALID!")
    }

    superConnector.close()
  }
}

object Checker {

  private val testAmountAssetName = "IIIuJIo"
  private val testPriceAssetName  = "MbIJIo"

  private val testAmountAssetDescription = "Amount asset for the Matcher checking purposes"
  private val testPriceAssetDescription  = "Price asset for the Matcher checking purposes"

  private val mnogo             = 100000000L
  private val testAssetDecimals = 8.toByte

  implicit class DoubleOps(private val value: Double) {
    val wuJIo, mbIJIo, waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}
