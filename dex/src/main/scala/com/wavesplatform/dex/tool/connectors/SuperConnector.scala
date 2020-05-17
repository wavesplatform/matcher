package com.wavesplatform.dex.tool.connectors

import java.io.File

import com.typesafe.config.ConfigFactory._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.settings.MatcherSettings.valueReader
import com.wavesplatform.dex.settings.{MatcherSettings, loadConfig}
import com.wavesplatform.dex.tool.connectors.SuperConnector.Env
import mouse.any._
import net.ceedubs.ficus.Ficus._

case class SuperConnector private (env: Env, dexRest: DexRestConnector, nodeRest: NodeRestConnector, dexExtensionGrpc: DexExtensionGrpcConnector)
    extends AutoCloseable {
  def close(): Unit = Seq(nodeRest, dexRest, dexExtensionGrpc).foreach { _.close() }
}

// noinspection ScalaStyle
object SuperConnector {

  private[tool] final case class Env(chainId: Byte, matcherSettings: MatcherSettings, matcherKeyPair: KeyPair)

  def wrapByLogs[T](begin: String, end: String = "Done")(f: => T): T = { print(begin); val result = f; println(end); result }

  def create(dexConfigPath: String, dexRestApi: String, nodeRestApi: String): SuperConnector = {

    val matcherSettings: MatcherSettings = wrapByLogs("Processing DEX config... ") {
      loadConfig { parseFile(new File(dexConfigPath)) }.as[MatcherSettings]("waves.dex")
    }

    import matcherSettings._

    AddressScheme.current = new AddressScheme { override val chainId: Byte = matcherSettings.addressSchemeCharacter.toByte }

    val chainId: Byte           = AddressScheme.current.chainId
    val matcherKeyPair: KeyPair = AccountStorage.load(matcherSettings.accountStorage).map(_.keyPair).explicitGet()

    val extensionGrpcApiUri: String = wavesBlockchainClient.grpc.target
    val dexRestApiUri: String       = s"http://${if (dexRestApi.nonEmpty) dexRestApi else s"${restApi.address}:${restApi.port}"}"
    val nodeRestApiUri: String      = s"http://${if (dexRestApi.nonEmpty) nodeRestApi else s"${extensionGrpcApiUri.dropRight(4) + 6869}"}"

    SuperConnector(
      Env(chainId, matcherSettings, matcherKeyPair),
      DexRestConnector(dexRestApiUri),
      NodeRestConnector(nodeRestApiUri, chainId),
      DexExtensionGrpcConnector(extensionGrpcApiUri, matcherKeyPair)
    ) unsafeTap { _ =>
      println(
        s"""
           |DEX configurations:
           |  Chain ID               : $chainId
           |  Matcher public key     : ${matcherKeyPair.publicKey.toString}
           |  Matcher address        : ${matcherKeyPair.publicKey.toAddress}
           |  DEX REST API           : $dexRestApiUri
           |  Node REST API          : $nodeRestApiUri
           |  DEX extension gRPC API : $extensionGrpcApiUri
       """.stripMargin
      )
    }
  }
}
