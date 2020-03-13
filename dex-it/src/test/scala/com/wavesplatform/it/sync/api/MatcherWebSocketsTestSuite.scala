package com.wavesplatform.it.sync.api

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.ws.Message
import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WebSocketConnection}
import com.wavesplatform.it.MatcherSuiteBase
import play.api.libs.json.Json

import scala.collection.mutable.ListBuffer

class MatcherWebSocketsTestSuite extends MatcherSuiteBase with HasWebSockets {

  private val carol = mkKeyPair("carol")

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()

    wavesNode1.restartWithNewSuiteConfig(
      ConfigFactory.parseString(
        s"""waves.dex.grpc.integration {
           |  balance-changes-batch-linger = 300ms
           |}
           |waves.miner.minimal-block-generation-offset = 30s
           |waves.blockchain.genesis {
           |  average-block-delay = 60s
           |}
           |""".stripMargin
      )
    )

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

  "Connection should be established" ignore {

    val wsUri                           = s"127.0.0.1:${dex1.restApiAddress.getPort}/ws/time"
    val outputParser: Message => String = _.asTextMessage.getStrictText

    val wscMobile = mkWebSocketConnection(wsUri, outputParser)
    val wscWeb    = mkWebSocketConnection(wsUri, outputParser, trackOutput = false)
    val wscTest   = mkWebSocketConnection(wsUri, outputParser)

    Thread.sleep(2000)

    val wscDesktop = mkWebSocketConnection(wsUri, outputParser)

    Thread.sleep(3000)

    Seq(wscMobile, wscDesktop, wscTest).foreach { connection =>
      connection.close()
      connection.getMessagesBuffer.foreach(_ should startWith("Now is"))
    }

    wscTest.clearMessagesBuffer()

    wscMobile.getMessagesBuffer.size should be > wscDesktop.getMessagesBuffer.size
    Seq(wscWeb, wscTest).foreach { _.getMessagesBuffer.size shouldBe 0 }
  }

  var messages = new ListBuffer[String]()

  for (i <- 1 to 100) s"test $i" in {
    val accS = createAccountWithBalance(150.006.waves -> Waves)
    val accB = createAccountWithBalance(225.usd -> usd)
    val wsc  = createAccountUpdatesWsConnection(accB)

    placeAndAwaitAtDex(mkOrderDP(accS, wavesUsdPair, SELL, 100.waves, 1.5))
    placeAndAwaitAtDex(mkOrderDP(accS, wavesUsdPair, SELL, 50.waves, 1.5))
    placeAndAwaitAtNode(mkOrderDP(accB, wavesUsdPair, BUY, 150.waves, 1.5))

    messages += wsc.getMessagesBuffer.mkString

    wsc.getMessagesBuffer should matchTo {
      Seq(
        WsAddressState(Map(Waves -> WsBalances(tradable = 0, reserved = 0), usd -> WsBalances(tradable = 225.usd, reserved = 0))),
        WsAddressState(Map(usd   -> WsBalances(tradable = 0, reserved = 225.usd))),
        WsAddressState(Map(usd   -> WsBalances(tradable = 0, reserved = 75.usd))),
        WsAddressState(Map(Waves -> WsBalances(tradable = 149.997.waves, reserved = 0), usd -> WsBalances(tradable = 0, reserved = 0)))
      )
    }
  }

  "print results" in {
    import java.io._
    val pw = new PrintWriter(new File("result.txt"))

    messages.toList.foreach(m => pw.write(s"$m\n"))
    pw.close
  }

  "MatcherWebSocketRoute" - {
    "should send account updates to authenticated user" ignore {

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
