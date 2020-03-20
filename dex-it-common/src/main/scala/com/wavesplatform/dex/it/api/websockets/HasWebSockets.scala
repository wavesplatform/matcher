package com.wavesplatform.dex.it.api.websockets

import java.lang
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import com.google.common.primitives.Longs
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrderBook}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.docker.DexContainer
import mouse.any._
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json.Json

trait HasWebSockets extends BeforeAndAfterAll { _: Suite =>

  private implicit val derivedAddressStateDiff: Diff[WsAddressState] = Derived(Diff.gen[WsAddressState].value.ignore(_.timestamp))
  private implicit val derivedOrderBookDiff: Diff[WsOrderBook]       = Derived(Diff.gen[WsOrderBook].value.ignore(_.timestamp))

  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)

  protected val knownWsConnections: ConcurrentHashMap.KeySetView[WebSocketConnection[_], lang.Boolean] =
    ConcurrentHashMap.newKeySet[WebSocketConnection[_]]()

  protected def addConnection(connection: WebSocketConnection[_]): Unit = knownWsConnections.add(connection)

  protected def mkWebSocketConnection[Output](uri: String,
                                              parseOutput: Message => Output,
                                              trackOutput: Boolean = true): WebSocketConnection[Output] = {
    new WebSocketConnection(uri, parseOutput, trackOutput = trackOutput) unsafeTap addConnection
  }

  protected def mkWebSocketAuthenticatedConnection(client: KeyPair, dex: DexContainer): WebSocketAuthenticatedConnection = {

    val prefix        = "as"
    val timestamp     = System.currentTimeMillis() + 86400000
    val signedMessage = prefix.getBytes(StandardCharsets.UTF_8) ++ client.publicKey.arr ++ Longs.toByteArray(timestamp)
    val signature     = com.wavesplatform.dex.domain.crypto.sign(client, signedMessage)
    val wsUri         = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/accountUpdates/${client.publicKey}?Timestamp=$timestamp&Signature=$signature"

    new WebSocketAuthenticatedConnection(wsUri)
  }

  protected def mkWebSocketOrderBookConnection(assetPair: AssetPair, dex: DexContainer): WebSocketConnection[WsOrderBook] = {
    val wsUri = s"127.0.0.1:${dex.restApiAddress.getPort}/ws/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}"
    mkWebSocketConnection(wsUri) { msg =>
      Json.parse(msg.asTextMessage.getStrictText).as[WsOrderBook]
    }
  }

  protected def mkWebSocketConnection[Output](uri: String)(parseOutput: Message => Output): WebSocketConnection[Output] = {
    new WebSocketConnection(uri, parseOutput, trackOutput = true) unsafeTap addConnection
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
        lastTrade = r.lastTrade.orElse(x.lastTrade)
      )
  }

  protected def squashBalanceChanges(xs: Seq[Map[Asset, WsBalances]]): Map[Asset, WsBalances] = xs.foldLeft(Map.empty[Asset, WsBalances]) { _ ++ _ }

  override def afterAll(): Unit = {
    super.afterAll()
    cleanupWebSockets()
  }
}
