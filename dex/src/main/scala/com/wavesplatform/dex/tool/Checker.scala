package com.wavesplatform.dex.tool

import java.io.File

import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.AddressScheme
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
import com.wavesplatform.wavesj.transactions.IssueTransactionV2
import com.wavesplatform.wavesj.{PrivateKeyAccount, Transactions}
import monix.execution.Scheduler.Implicits.{global => monixScheduler}
import net.ceedubs.ficus.Ficus._
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import sttp.client._

import scala.concurrent.ExecutionContext.Implicits.{global => executionContext}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

// noinspection ScalaStyle
case class Checker(dexRestApi: String, nodeRestApiUri: String, version: String, dexConfigPath: String) {

  import Checker._

  LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.OFF)

  println(
    s"""
       |Passed arguments:
       |  DEX REST API         : $dexRestApi
       |  WavesNode REST API   : $nodeRestApiUri
       |  Expected DEX version : $version
       |  DEX config path      : $dexConfigPath
       """.stripMargin
  )

  private implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  print(s"Processing DEX config... ")

  private val config: Config              = parseFile(new File(dexConfigPath))
  private val settings: MatcherSettings   = loadConfig(config).as[MatcherSettings]("waves.dex")
  private val matcherKeyPair              = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet()
  private val dexRestApiUri: String       = s"http://${if (dexRestApi.nonEmpty) dexRestApi else s"${settings.restApi.address}:${settings.restApi.port}"}/"
  private val extensionGrpcApiUri: String = settings.wavesBlockchainClient.grpc.target

  AddressScheme.current = new AddressScheme { override val chainId: Byte = settings.addressSchemeCharacter.toByte }

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
       | Address scheme         : ${AddressScheme.current.chainId}
       | Matcher public key     : ${matcherKeyPair.publicKey.toString}
       | Matcher address        : ${matcherKeyPair.publicKey.toAddress}
       | DEX REST API           : $dexRestApiUri
       | DEX extension gRPC API : $extensionGrpcApiUri
       """.stripMargin
  )

  private def mkIssueAssetTx(name: String, description: String, quantity: Long): IssueTransactionV2 = {
    val matcherAccount = PrivateKeyAccount.fromPrivateKey(matcherKeyPair.privateKey.base58, AddressScheme.current.chainId)
    Transactions.makeIssueTx(matcherAccount, AddressScheme.current.chainId, name, description, quantity, 8, false, null, 1.waves)
  }

  private def issueTestAssets(): Unit = {
    val mnogo       = 3000000
    val issueAwlTx  = mkIssueAssetTx("IIIuJIo", "Awl for the Matcher checking purposes", mnogo.wuJIo)
    val issueSoapTx = mkIssueAssetTx("MbIJIo", "Soap for the Matcher checking purposes", mnogo.mbIJIo)
  }

  private def checkVersion(): Boolean = {
    print(s" 1. DEX version... ")

    val uri      = uri"$dexRestApiUri/api-docs/swagger.json"
    val request  = basicRequest.get(uri)
    val response = request.send().body

    response.fold(
      error => { println(s"Cannot parse $uri! $error"); false },
      body => {
        val parsedVersion = (Json.parse(body) \ "info" \ "version").get.as[String].trim
        val checkResult   = parsedVersion == version
        println(if (checkResult) "Passed" else s"""Failed! Expected "$version", but got "$parsedVersion"""")
        checkResult
      }
    )
  }

  private def checkTestAssets(): Boolean = {
    println(s" 2. Test assets existence")

    def getDetailedBalance(asset: Asset, balance: Long): Future[(Asset, (BriefAssetDescription, Long))] = {
      asset match {
        case Waves           => Future.successful(asset -> (BriefAssetDescription.wavesDescription -> balance))
        case ia: IssuedAsset => grpcAsyncClient.assetDescription(ia).map(maybeDesc => ia -> (maybeDesc.get -> balance))
      }
    }

    val matcherBalance =
      Await.result(
        for {
          balances                <- grpcAsyncClient.allAssetsSpendableBalance(matcherKeyPair.toAddress)
          balancesWithDescription <- balances.toList.traverse { case (a, b) => getDetailedBalance(a, b) }
        } yield balancesWithDescription.toMap,
        10.seconds
      )

    def checkAssetExistence(name: String): Boolean = {
      matcherBalance.find(_._2._1.name == name).fold { println("Not found"); false } {
        case (a, (d, b)) =>
          val denormalizedBalance = Denormalization.denormalizeAmountAndFee(b, 8)
          println(s"Found, name = ${d.name}, id = ${a.toString}, balance = $denormalizedBalance")
          true
      }
    }

    print(s"   2.1 Amount asset... "); val amountAssetResult = checkAssetExistence(testAmountAssetName)
    print(s"   2.2 Price asset... "); val priceAssetResult   = checkAssetExistence(testPriceAssetName)

    amountAssetResult && priceAssetResult
  }

  def checkState(): Unit = {
    println(s"Checking")
    val isStateCorrect = checkVersion() && checkTestAssets()
    println(s"\n${if (isStateCorrect) "Congratulations! DEX state is valid!" else "DEX state is INVALID!"}\n")
    grpcAsyncClient.close()
  }
}

object Checker {

  private val testAmountAssetName = "IIIuJIo"
  private val testPriceAssetName  = "MbIJo"

  implicit class DoubleOps(private val value: Double) {
    val wuJIo, mbIJIo, waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}
