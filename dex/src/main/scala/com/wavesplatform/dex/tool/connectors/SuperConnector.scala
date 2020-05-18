package com.wavesplatform.dex.tool.connectors

import java.io.File

import cats.syntax.either._
import cats.syntax.option._
import com.typesafe.config.ConfigFactory._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.settings.MatcherSettings.valueReader
import com.wavesplatform.dex.settings.{MatcherSettings, loadConfig}
import com.wavesplatform.dex.tool._
import com.wavesplatform.dex.tool.connectors.SuperConnector.Env
import net.ceedubs.ficus.Ficus._

import scala.util.Try

case class SuperConnector private (env: Env, dexRest: DexRestConnector, nodeRest: NodeRestConnector, dexExtensionGrpc: DexExtensionGrpcConnector)
    extends AutoCloseable {

  def close(): Unit = Seq(nodeRest, dexRest, dexExtensionGrpc).foreach { _.close() }
}

// noinspection ScalaStyle
object SuperConnector {

  private[tool] final case class Env(chainId: Byte, matcherSettings: MatcherSettings, matcherKeyPair: KeyPair)

  private val processLeftIndent = 90

  def create(dexConfigPath: String, nodeRestApi: String): ErrorOr[SuperConnector] = {

    def logProcessing[A](processing: String)(f: => ErrorOr[A]): ErrorOr[A] = wrapByLogs(f)(s"  $processing... ", "Done\n", processLeftIndent.some)

    def loadMatcherSettings(confPath: String): ErrorOr[MatcherSettings] =
      Try {
        val matcherSettings: MatcherSettings = loadConfig { parseFile(new File(dexConfigPath)) }.as[MatcherSettings]("waves.dex")
        AddressScheme.current = new AddressScheme { override val chainId: Byte = matcherSettings.addressSchemeCharacter.toByte }
        matcherSettings
      }.toEither.leftMap(ex => s"Cannot load matcher settings by path $confPath: $ex")

    def loadMatcherKeyPair(accountStorage: AccountStorage.Settings): ErrorOr[KeyPair] =
      AccountStorage.load(accountStorage).bimap(ex => s"Cannot load Matcher account! $ex", _.keyPair)

    for {
      _               <- log("\nSetting up the Super Connector:\n")
      matcherSettings <- logProcessing("Loading Matcher settings") { loadMatcherSettings(dexConfigPath) }
      matcherKeyPair  <- logProcessing("Loading Matcher key pair") { loadMatcherKeyPair(matcherSettings.accountStorage) }

      dexRestApiUri    = s"http://${matcherSettings.restApi.address}:${matcherSettings.restApi.port}"
      dexRestConnector = DexRestConnector(dexRestApiUri)

      _ <- logProcessing(s"Setting up connection with DEX REST API (${dexRestConnector.repeatRequestOptions})") {
        dexRestConnector.waitForSwaggerJson
      }

      chainId           = AddressScheme.current.chainId
      nodeRestApiUri    = if (nodeRestApi.startsWith("https://") || nodeRestApi.startsWith("http://")) nodeRestApi else s"http://$nodeRestApi"
      nodeRestConnector = NodeRestConnector(nodeRestApiUri, chainId)

      _ <- logProcessing(s"Setting up connection with Node REST API (${nodeRestConnector.repeatRequestOptions})") {
        nodeRestConnector.waitForSwaggerJson
      }

      extensionGrpcApiUri = matcherSettings.wavesBlockchainClient.grpc.target

      dexExtensionGrpcConnector <- logProcessing("Setting up connection with DEX Extension gRPC API") {
        DexExtensionGrpcConnector.create(extensionGrpcApiUri)
      }

      env            = Env(chainId, matcherSettings, matcherKeyPair)
      superConnector = SuperConnector(env, dexRestConnector, nodeRestConnector, dexExtensionGrpcConnector)

      _ <- log(
        s"""
           |Super Connector created!
           |
           |DEX configurations:
           |  Chain ID               : $chainId
           |  Matcher public key     : ${matcherKeyPair.publicKey.toString}
           |  Matcher address        : ${matcherKeyPair.publicKey.toAddress}
           |  DEX REST API           : $dexRestApiUri
           |  Node REST API          : $nodeRestApi
           |  DEX extension gRPC API : $extensionGrpcApiUri
       """.stripMargin
      )
    } yield superConnector
  }
}
