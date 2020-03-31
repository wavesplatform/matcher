package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap
import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import com.google.common.primitives.Longs
import com.wavesplatform.dex.api.websockets.{WsBalances, WsOrderBook}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.dex.it.docker.apiKey
import mouse.any._
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json.Json

trait HasWebSockets extends BeforeAndAfterAll { _: Suite =>
  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)

  protected def getBaseBalancesStreamUri(dex: DexContainer): String   = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/accountUpdates/"
  protected def getBaseOrderBooksStreamUri(dex: DexContainer): String = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/orderbook/"

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WsConnection[_], lang.Boolean] =
    ConcurrentHashMap.newKeySet[WsConnection[_]]()

  protected def addConnection(connection: WsConnection[_]): Unit = knownWsConnections.add(connection)

  protected def mkWebSocketConnection[Output](uri: String,
                                              parseOutput: Message => Output,
                                              trackOutput: Boolean = true): WsConnection[Output] = {
    new WsConnection(uri, parseOutput, trackOutput = trackOutput) unsafeTap addConnection
  }

  protected def mkWsAuthenticatedConnection(client: KeyPair, dex: DexContainer): WsAuthenticatedConnection = {

    val prefix        = "as"
    val timestamp     = System.currentTimeMillis() + 86400000
    val signedMessage = prefix.getBytes(StandardCharsets.UTF_8) ++ client.publicKey.arr ++ Longs.toByteArray(timestamp)
    val signature     = com.wavesplatform.dex.domain.crypto.sign(client, signedMessage)
    val wsUri         = s"${getBaseBalancesStreamUri(dex)}${client.publicKey}?t=$timestamp&s=$signature"

    new WsAuthenticatedConnection(wsUri, None)
  }

  protected def mkWsAuthenticatedConnectionViaApiKey(client: KeyPair,
                                                     dex: DexContainer,
                                                     apiKey: String = apiKey): WsAuthenticatedConnection = {
    val timestamp = System.currentTimeMillis() + 86400000
    val wsUri     = s"${getBaseBalancesStreamUri(dex)}${client.publicKey}?t=$timestamp"

    new WsAuthenticatedConnection(wsUri, Some(apiKey))
  }

  protected def mkWebSocketOrderBookConnection(assetPair: AssetPair, dex: DexContainer): WsConnection[WsOrderBook] = {
    val wsUri = s"${getBaseOrderBooksStreamUri(dex)}${assetPair.amountAssetStr}/${assetPair.priceAssetStr}"
    mkWebSocketConnection(wsUri) { msg =>
      Json.parse(msg.asTextMessage.getStrictText).as[WsOrderBook]
    }
  }

  protected def mkWebSocketConnection[Output](uri: String)(parseOutput: Message => Output): WsConnection[Output] = {
    new WsConnection(uri, parseOutput, trackOutput = true) unsafeTap addConnection
  }

  protected def cleanupWebSockets(): Unit = {
    if (!knownWsConnections.isEmpty) {
      knownWsConnections.forEach(c => if (!c.isClosed) c.close())
      materializer.shutdown()
    }
  }

  protected def squashOrderBooks(xs: TraversableOnce[WsOrderBook]): WsOrderBook = xs.foldLeft(WsOrderBook.empty) {
    case (r, x) =>
      WsOrderBook(
        asks = r.asks ++ x.asks,
        bids = r.bids ++ x.bids,
        lastTrade = r.lastTrade.orElse(x.lastTrade),
        timestamp = xs.toList.last.timestamp
      )
  }

  protected def squashBalanceChanges(xs: Seq[Map[Asset, WsBalances]]): Map[Asset, WsBalances] = xs.foldLeft(Map.empty[Asset, WsBalances]) { _ ++ _ }

  override def afterAll(): Unit = {
    super.afterAll()
    cleanupWebSockets()
  }
}
