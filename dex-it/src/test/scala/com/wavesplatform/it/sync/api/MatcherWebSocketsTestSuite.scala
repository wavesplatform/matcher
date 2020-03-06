package com.wavesplatform.it.sync.api

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WebSocketConnection}
import com.wavesplatform.it.MatcherSuiteBase
import play.api.libs.json.Json

class MatcherWebSocketsTestSuite extends MatcherSuiteBase with HasWebSockets {

  private val carol = mkKeyPair("carol")

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, carol, 100.waves, Waves), mkTransfer(bob, carol, 1.btc, btc))
    dex1.start()
  }

  private def createAccountUpdatesWsConnection(client: KeyPair): WebSocketConnection[WsAddressState] = {
    val prefix        = "as"
    val timestamp     = System.currentTimeMillis()
    val signedMessage = prefix.getBytes(StandardCharsets.UTF_8) ++ client.publicKey.arr ++ Longs.toByteArray(timestamp)
    val signature     = com.wavesplatform.dex.domain.crypto.sign(client, signedMessage)
    val wsUri         = s"127.0.0.1:${dex1.restApiAddress.getPort}/ws/accountUpdates/${client.publicKey}?Timestamp=$timestamp&Signature=$signature"

    mkWebSocketConnection(wsUri, msg => WsAddressState.format.reads(Json parse msg.asTextMessage.getStrictText).get)
  }

  "Connection should be established" in {
    val wsc = createAccountUpdatesWsConnection(alice)
    wsc.close()
    wsc.getMessagesBuffer.foreach { x =>
      x.balances should not be empty
    }
  }

  "MatcherWebSocketRoute" - {

    "should send account updates to authenticated user" in {

      // Carol has 100 Waves and 1 BTC
      val wsc = createAccountUpdatesWsConnection(carol)

      placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00011403))
      placeAndAwaitAtDex(mkOrderDP(carol, wavesUsdPair, SELL, 1.waves, 3.01))
      dex1.api.cancelAll(carol)

      Thread.sleep(300)

      wsc.getMessagesBuffer should matchTo {
        Seq(
          WsAddressState(Map(Waves -> WsBalances(tradable = 100.waves, reserved = 0), btc -> WsBalances(tradable = 1.btc, reserved = 0))),
          WsAddressState(Map(btc   -> WsBalances(tradable = 0.99988597.btc, reserved = 0.00011403.btc))),
          WsAddressState(Map(Waves -> WsBalances(tradable = 98.997.waves, reserved = 1.003.waves))),
          WsAddressState(Map(Waves -> WsBalances(tradable = 100.waves, reserved = 0), btc -> WsBalances(tradable = 1.btc, reserved = 0)))
        )
      }
    }
  }

}
