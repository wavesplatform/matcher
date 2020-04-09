package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import com.google.common.primitives.Longs
import com.wavesplatform.dex.api.websockets.{WsMessage, WsOrderBook}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.it.docker.{DexContainer, apiKey}
import mouse.any._
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json.Json

import scala.concurrent.duration._
import scala.reflect.ClassTag

trait HasWebSockets extends BeforeAndAfterAll { _: Suite =>

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)

  protected val authenticatedStreamSignaturePrefix = "au"

  protected def getBaseBalancesStreamUri(dex: DexContainer): String   = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/accountUpdates/"
  protected def getBaseOrderBooksStreamUri(dex: DexContainer): String = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/orderbook/"

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WsConnection[_], lang.Boolean] =
    ConcurrentHashMap.newKeySet[WsConnection[_]]()

  protected def addConnection(connection: WsConnection[_]): Unit = knownWsConnections.add(connection)

  protected def mkWsAuthenticatedConnection(client: KeyPair, dex: DexContainer, keepAlive: Boolean = true): WsAuthenticatedConnection = {

    val timestamp     = System.currentTimeMillis() + 1.hour.toMillis
    val signedMessage = authenticatedStreamSignaturePrefix.getBytes(StandardCharsets.UTF_8) ++ client.publicKey.arr ++ Longs.toByteArray(timestamp)
    val signature     = com.wavesplatform.dex.domain.crypto.sign(client, signedMessage)
    val wsUri         = s"${getBaseBalancesStreamUri(dex)}${client.publicKey}?t=$timestamp&s=$signature"

    new WsAuthenticatedConnection(wsUri, None, keepAlive) unsafeTap addConnection
  }

  protected def mkWsAuthenticatedConnectionViaApiKey(client: KeyPair,
                                                     dex: DexContainer,
                                                     apiKey: String = apiKey,
                                                     keepAlive: Boolean = true): WsAuthenticatedConnection = {
    val wsUri = s"${getBaseBalancesStreamUri(dex)}${client.publicKey}"
    new WsAuthenticatedConnection(wsUri, Some(apiKey), keepAlive) unsafeTap addConnection
  }

  protected def mkWsOrderBookConnection(assetPair: AssetPair, dex: DexContainer): WsConnection[WsOrderBook] = {
    val wsUri = s"${getBaseOrderBooksStreamUri(dex)}${assetPair.amountAssetStr}/${assetPair.priceAssetStr}"
    mkWsConnection(wsUri) { msg =>
      Json.parse(msg.asTextMessage.getStrictText).as[WsOrderBook]
    }
  }

  protected def mkWsConnection[Output <: WsMessage: ClassTag](uri: String)(parseOutput: Message => Output): WsConnection[Output] = {
    new WsConnection(uri, parseOutput, trackOutput = true) unsafeTap addConnection
  }

  protected def cleanupWebSockets(): Unit = {
    if (!knownWsConnections.isEmpty) {
      knownWsConnections.forEach { _.close() }
      materializer.shutdown()
    }
  }

  override def afterAll(): Unit = {
    super.afterAll()
    cleanupWebSockets()
  }
}
