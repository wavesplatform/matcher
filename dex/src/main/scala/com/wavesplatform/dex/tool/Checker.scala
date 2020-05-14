package com.wavesplatform.dex.tool

import java.io.File

import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.model.{Denormalization, Normalization}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.WavesBlockchainClientBuilder
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.settings.GrpcClientSettings._
import com.wavesplatform.dex.grpc.integration.settings.{GrpcClientSettings, WavesBlockchainClientSettings}
import com.wavesplatform.dex.settings.MatcherSettings.valueReader
import com.wavesplatform.dex.settings.{MatcherSettings, loadConfig}
import com.wavesplatform.wavesj.json.WavesJsonMapper
import com.wavesplatform.wavesj.{PrivateKeyAccount, Transactions}
import monix.execution.Scheduler.Implicits.{global => monixScheduler}
import net.ceedubs.ficus.Ficus._
import org.slf4j.LoggerFactory
import play.api.libs.json.jackson.PlayJsonModule
import play.api.libs.json.{Json, JsonParserSettings}
import sttp.client._
import sttp.model.MediaType

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

// noinspection ScalaStyle
case class Checker(dexRestApi: String, nodeRestApi: String, version: String, dexConfigPath: String) {

  import Checker._

  LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.OFF)

  println(
    s"""
       |Passed arguments:
       |  DEX REST API         : $dexRestApi
       |  WavesNode REST API   : $nodeRestApi
       |  Expected DEX version : $version
       |  DEX config path      : $dexConfigPath
       """.stripMargin
  )

  private implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  type CheckResult[A] = Either[String, A]

  private def denormalize(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, 8).toDouble

  private def check[A](name: String, doneOnSameLine: Boolean = true)(f: => CheckResult[A]): CheckResult[A] = {
    if (doneOnSameLine) print(name) else println(name)
    val res = f
    val msg = res.fold(identity, { case () => "Done"; case s: String => s })
    println(msg)
    res
  }

  print(s"Processing DEX config... ")

  private val config: Config              = parseFile(new File(dexConfigPath))
  private val settings: MatcherSettings   = loadConfig(config).as[MatcherSettings]("waves.dex")
  private val matcherKeyPair: KeyPair     = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet()
  private val extensionGrpcApiUri: String = settings.wavesBlockchainClient.grpc.target

  private val dexRestApiUri: String  = s"http://${if (dexRestApi.nonEmpty) dexRestApi else s"${settings.restApi.address}:${settings.restApi.port}"}"
  private val nodeRestApiUri: String = s"http://${if (dexRestApi.nonEmpty) nodeRestApi else s"${extensionGrpcApiUri.dropRight(4) + 6869}"}"

  AddressScheme.current = new AddressScheme { override val chainId: Byte = settings.addressSchemeCharacter.toByte }

  private val chainId: Byte = AddressScheme.current.chainId

  private val matcherAccount: PrivateKeyAccount = PrivateKeyAccount.fromPrivateKey(matcherKeyPair.privateKey.base58, chainId)
  private val mapper: WavesJsonMapper           = new WavesJsonMapper(chainId); mapper.registerModule(new PlayJsonModule(JsonParserSettings()))

  println("Done")

  private val grpcAsyncClient: WavesBlockchainClient[Future] = {
    print("Establishing gRPC connection to DEX extension... ")
    val grpcSettings   = GrpcClientSettings(extensionGrpcApiUri, 5, 5, true, 2.seconds, 5.seconds, 1.minute, ChannelOptionsSettings(5.seconds))
    val clientSettings = WavesBlockchainClientSettings(grpcSettings, 100.milliseconds, 100)
    val client         = WavesBlockchainClientBuilder.async(clientSettings, monixScheduler, executionContext)
    println(s"Done")
    client
  }

  println(
    s"""
       |DEX configurations:
       | Chain ID               : $chainId
       | Matcher public key     : ${matcherKeyPair.publicKey.toString}
       | Matcher address        : ${matcherKeyPair.publicKey.toAddress}
       | DEX REST API           : $dexRestApiUri
       | Node REST API          : $nodeRestApiUri
       | DEX extension gRPC API : $extensionGrpcApiUri
       """.stripMargin
  )

  private def checkVersion(checkName: String): CheckResult[String] = check(s" $checkName ") {
    basicRequest.get(uri"$dexRestApiUri/api-docs/swagger.json").send().body.flatMap { body =>
      val parsedVersion = (Json.parse(body) \ "info" \ "version").get.as[String].trim
      Either.cond(parsedVersion == version, "Passed", s"""Failed! Expected "$version", but got "$parsedVersion"""")
    }
  }

  private def issueTestAsset(name: String, description: String, quantity: Long): CheckResult[String] = {

    val tx           = Transactions.makeIssueTx(matcherAccount, chainId, name, description, quantity, testAssetDecimals, false, null, 1.waves)
    val serializedTx = mapper writeValueAsString tx

    def broadcastRequest = basicRequest.post(uri"$nodeRestApiUri/transactions/broadcast").body(serializedTx).contentType(MediaType.ApplicationJson)
    def txInfoRequest    = basicRequest.get(uri"$nodeRestApiUri/transactions/info/${tx.getId.toString}")

    @tailrec
    def repeatRequest(attemptsLeft: Int, delay: FiniteDuration)(sendRequest: => Either[String, String],
                                                                test: Either[String, String] => Boolean): Either[String, String] = {
      if (attemptsLeft == 0) "All attempts are out!".asLeft
      else {
        val response = sendRequest
        if (test(response)) response
        else {
          Thread.sleep(delay.toMillis)
          repeatRequest(attemptsLeft - 1, delay)(sendRequest, test)
        }
      }
    }

    for {
      _ <- { print(s"Issuing ${denormalize(quantity)} $name... "); broadcastRequest.send().body.leftMap(ex => s"Cannot broadcast transaction! $ex") }
      _ <- { print(s"Awaiting tx in blockchain... "); repeatRequest(10, 1.second)(txInfoRequest.send().body, _.isRight) }
    } yield "Issued!"
  }

  private def checkTestAssets(checkName: String): CheckResult[String] = check(s" $checkName ", doneOnSameLine = false) {

    def getDetailedBalance(asset: Asset, balance: Long): Future[(Asset, (BriefAssetDescription, Long))] = asset match {
      case Waves           => Future.successful(asset -> (BriefAssetDescription.wavesDescription -> balance))
      case ia: IssuedAsset => grpcAsyncClient.assetDescription(ia).map(maybeDesc => ia -> (maybeDesc.get -> balance))
    }

    val matcherBalance =
      Await.result(
        for {
          balances                <- grpcAsyncClient.allAssetsSpendableBalance(matcherKeyPair.toAddress)
          balancesWithDescription <- balances.toList.traverse { case (a, b) => getDetailedBalance(a, b) }
        } yield balancesWithDescription.toMap,
        10.seconds
      )

    def findOrIssue(checkName: String, assetName: String, description: String, quantity: Long): CheckResult[String] = check(checkName) {
      matcherBalance
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

  def checkState(): Unit = {
    println(s"Checking")

    val checkResult =
      for {
        _ <- checkVersion("1. DEX version...")
        _ <- checkTestAssets("2. Test assets existence...")
      } yield ()

    checkResult match {
      case Right(_) => println("\nCongratulations! DEX state is valid!")
      case Left(_)  => println(s"\nDEX state is INVALID!")
    }

    grpcAsyncClient.close()
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
