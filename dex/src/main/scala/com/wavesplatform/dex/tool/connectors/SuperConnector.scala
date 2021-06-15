package com.wavesplatform.dex.tool.connectors

import cats.instances.either._
import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.cli._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.tool.connectors.Connector.RepeatRequestOptions
import com.wavesplatform.dex.tool.connectors.SuperConnector.Env

import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class SuperConnector private (
  env: Env,
  dexRest: DexRestConnector,
  nodeRest: NodeRestConnector,
  dexExtensionGrpc: DexExtensionGrpcConnector,
  dexWs: DexWsConnector,
  authServiceRest: Option[AuthServiceRestConnector]
) extends AutoCloseable {

  def close(): Unit = {
    Seq(dexRest, nodeRest, dexExtensionGrpc, dexWs).foreach(_.close())
    authServiceRest.foreach(_.close())
  }

}

// noinspection ScalaStyle
object SuperConnector {

  final private[tool] case class Env(chainId: Byte, matcherSettings: MatcherSettings, matcherKeyPair: KeyPair)

  private val processLeftIndent = 110

  def create(
    matcherSettings: MatcherSettings,
    dexRestApi: String,
    nodeRestApi: String,
    authServiceRestApi: Option[String],
    apiKey: String
  ): ErrorOr[SuperConnector] = {

    def prependScheme(uri: String): String = {
      val uriWithoutSlash = if (uri.last == '/') uri.init else uri
      val (plain, secure) = "http://" -> "https://"
      if (uri.startsWith(secure) || uri.startsWith(plain)) uriWithoutSlash else plain + uriWithoutSlash
    }

    def logProcessing[A](processing: String)(f: => ErrorOr[A]): ErrorOr[A] = wrapByLogs(s"  $processing... ", "Done\n", processLeftIndent.some)(f)

    def loadMatcherKeyPair(accountStorage: AccountStorage.Settings): ErrorOr[KeyPair] =
      AccountStorage.load(accountStorage).bimap(ex => s"Cannot load Matcher account! $ex", _.keyPair)

    AddressScheme.current = new AddressScheme { override val chainId: Byte = matcherSettings.addressSchemeCharacter.toByte }

    def waitUntilMatcherStarts(dexRest: DexRestConnector, apiKey: String, waitingTime: FiniteDuration): ErrorOr[Unit] =
      dexRest.repeatRequest(dexRest.getMatcherStatus(apiKey)) { response =>
        response.isLeft || response.exists { jsValue =>
          (jsValue \ "service").asOpt[String].contains("Working") &&
          (jsValue \ "blockchain" \ "status").asOpt[String].contains("Working")
        }
      }(RepeatRequestOptions(waitingTime.toSeconds.toInt, 1.second)).map(_ => ())

    for {
      _ <- log("\nSetting up the Super Connector:\n")
      matcherKeyPair <- logProcessing("Loading Matcher Key Pair")(loadMatcherKeyPair(matcherSettings.accountStorage))

      chainId = AddressScheme.current.chainId
      nodeRestApiUri = prependScheme(
        if (nodeRestApi.isEmpty) matcherSettings.wavesBlockchainClient.grpc.target.split(":").head + ":6869" else nodeRestApi
      )
      nodeRestConnector = NodeRestConnector(nodeRestApiUri, chainId)
      _ <- logProcessing(s"Setting up connection with Node REST API (${nodeRestConnector.repeatRequestOptions})")(success)

      dexRestIpAndPort = s"${matcherSettings.restApi.address}:${matcherSettings.restApi.port}"
      dexRestApiUri = prependScheme(if (dexRestApi.isEmpty) dexRestIpAndPort else dexRestApi)
      dexRestConnector = DexRestConnector(dexRestApiUri)
      _ <- logProcessing(s"Setting up connection with DEX REST API (${dexRestConnector.repeatRequestOptions})")(success)

      waitingTime = {
        matcherSettings.snapshotsLoadingTimeout +
        matcherSettings.startEventsProcessingTimeout +
        matcherSettings.orderBooksRecoveringTimeout
      }
      _ <- logProcessing(s"Wait until matcher starts ($waitingTime)")(waitUntilMatcherStarts(dexRestConnector, apiKey, waitingTime))

      extensionGrpcApiUri = matcherSettings.wavesBlockchainClient.grpc.target
      blockchainUpdatesExtensionGrpcApiUri = matcherSettings.wavesBlockchainClient.blockchainUpdatesGrpc.target

      dexExtensionGrpcConnector <- logProcessing("Setting up connection with DEX Extension gRPC API") {
        DexExtensionGrpcConnector.create(
          matcherKeyPair.publicKey,
          extensionGrpcApiUri,
          blockchainUpdatesExtensionGrpcApiUri
        )
      }

      dexWsApiUri = {
        val protocol = if (dexRestApiUri.startsWith("http://")) "ws" else "wss"
        s"$protocol://$dexRestIpAndPort/ws/v0"
      }

      (dexWsConnector, wsInitial) <- logProcessing("Setting up connection with DEX WS API")(
        for {
          wsConnector <- DexWsConnector.create(dexWsApiUri)
          wsInitial <- wsConnector.receiveInitialMessage
        } yield wsConnector -> wsInitial
      )

      mayBeAuthServiceRestApiUri = authServiceRestApi map prependScheme
      mayBeAuthServiceConnector = mayBeAuthServiceRestApiUri.map(AuthServiceRestConnector(_, chainId))
      _ <- logProcessing("Setting up connection with Auth Service REST API")(success)

      env = Env(chainId, matcherSettings, matcherKeyPair)
      superConnector = SuperConnector(
        env,
        dexRestConnector,
        nodeRestConnector,
        dexExtensionGrpcConnector,
        dexWsConnector,
        mayBeAuthServiceConnector
      )

      _ <- log(
        s"""
           |Super Connector created!
           |
           |DEX configurations:
           |  Chain ID               : $chainId (${chainId.toChar})
           |  Matcher public key     : ${matcherKeyPair.publicKey.toString}
           |  Matcher address        : ${matcherKeyPair.publicKey.toAddress}
           |  DEX REST API           : $dexRestApiUri
           |  Node REST API          : $nodeRestApiUri
           |  Matcher extension gRPC API            : $extensionGrpcApiUri
           |  Blockchain Updates extension gRPC API : $blockchainUpdatesExtensionGrpcApiUri
           |  DEX WS API             : $dexWsApiUri, connection ID = ${wsInitial.connectionId}
           |  Auth Service REST API  : ${mayBeAuthServiceRestApiUri.getOrElse(
          "Target wasn't provided, account updates check will not be performed!"
        )}
       """.stripMargin
      )
    } yield superConnector
  }

}
