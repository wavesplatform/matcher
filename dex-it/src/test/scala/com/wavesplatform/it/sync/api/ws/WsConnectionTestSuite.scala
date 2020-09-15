package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.it.WsSuiteBase

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class WsConnectionTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex.price-assets = [ "$BtcId", "$UsdId", "WAVES" ]""")
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  "Matcher should send rates snapshot after initial message and rates updates" in {

    def assertRatesUpdates(wsc: WsConnection)(expectedRatesUpdates: List[(Map[Asset, Double], Long)]): Unit = {
      wsc
        .receiveAtLeastN[WsRatesUpdates](expectedRatesUpdates.size)
        .map(r => r.rates -> r.updateId) should matchTo(expectedRatesUpdates)
    }

    dex1.api.upsertRate(btc, 0.00041863)

    val wsc1 = mkDexWsConnectionWithInitialMessage(dex1); wsc1.receiveAtLeastN[WsInitial](1)

    withClue("Rates snapshot") {
      assertRatesUpdates(wsc1) { List((Map(Waves -> 1, btc -> 0.00041863), 0)) }
      wsc1.clearMessages()
    }

    dex1.api.upsertRate(btc, 0.00041864)

    val wsc2 = mkDexWsConnectionWithInitialMessage(dex1); wsc2.receiveAtLeastN[WsInitial](1)

    dex1.api.upsertRate(usd, 2.76)

    withClue("Rates update") {
      assertRatesUpdates(wsc1) {
        List(
          Map[Asset, Double](btc -> 0.00041864) -> 1L,
          Map[Asset, Double](usd -> 2.76)       -> 2L
        )
      }
      assertRatesUpdates(wsc2) {
        List(
          Map[Asset, Double](Waves -> 1, btc -> 0.00041864) -> 0L,
          Map[Asset, Double](usd   -> 2.76) -> 1L
        )
      }
      Seq(wsc1, wsc2).foreach { _.clearMessages() }
    }

    Seq(btc, usd).foreach(dex1.api.deleteRate)

    withClue("Rates delete") {
      assertRatesUpdates(wsc1) {
        List(
          Map[Asset, Double](btc -> -1) -> 3L,
          Map[Asset, Double](usd -> -1) -> 4L
        )
      }
      assertRatesUpdates(wsc2) {
        List(
          Map[Asset, Double](btc -> -1) -> 2L,
          Map[Asset, Double](usd -> -1) -> 3L
        )
      }
      Seq(wsc1, wsc2).foreach { _.clearMessages() }
    }

    withClue("Rates snapshot after deleting") {
      val wsc = mkDexWsConnectionWithInitialMessage(dex1); wsc.receiveAtLeastN[WsInitial](1)
      assertRatesUpdates(wsc) { List(Map[Asset, Double](Waves -> 1) -> 0) }
      wsc.close()
    }

    Seq(wsc1, wsc2).foreach { _.close() }
  }

  "Updates both from address and order book" in {
    val wsc = mkDexWsConnection(dex1)

    markup("Subscribe to an order book updates")
    wsc.send(WsOrderBookSubscribe(wavesBtcPair, 1))
    wsc.receiveAtLeastN[WsOrderBookChanges](1)
    wsc.clearMessages()

    markup("Subscribe to an address updates")
    wsc.send(WsAddressSubscribe(alice, WsAddressSubscribe.defaultAuthType, mkJwt(alice)))
    wsc.receiveAtLeastN[WsAddressChanges](1)
    wsc.clearMessages()

    markup("Place an order")
    val order = mkOrderDP(alice, wavesBtcPair, SELL, 1.waves, 0.00005)
    placeAndAwaitAtDex(order)
    wsc.receiveAtLeastN[WsOrderBookChanges](1)
    wsc.receiveAtLeastN[WsAddressChanges](1)
    wsc.clearMessages()

    markup("Unsubscribe from an address updates")
    wsc.send(WsUnsubscribe(alice))

    markup("Cancel an order")
    cancelAndAwait(alice, order)
    wsc.receiveAtLeastN[WsOrderBookChanges](1)
    wsc.receiveNoMessagesOf[WsAddressChanges]()

    wsc.close()
  }

  "Matcher should handle many connections simultaneously" in {
    Await.result(Future.traverse((1 to 200).toList)(_ => Future(mkDexWsConnection(dex1))), 25.seconds).foreach { wsc =>
      wsc.isClosed shouldBe false
      wsc.close()
    }
  }

  "getConnections returns the right number of connections" in {
    val wscs = (1 to 10).map(_ => mkDexWsConnection(dex1))
    dex1.api.waitForWsConnections(_.connections == 10)
    wscs.foreach(_.close())
  }

  "closeConnection closes N oldest connections" in {
    val wscs = (1 to 10).map(_ => mkDexWsConnection(dex1))
    dex1.api.waitForWsConnections(_.connections == 10)

    dex1.api.closeWsConnections(3)
    dex1.api.waitForWsConnections(_.connections == 7)

    val (closed, active) = wscs.splitAt(3)

    withClue("closed\n") {
      closed.foreach { x =>
        eventually {
          x.isClosed shouldBe true
        }
      }
    }

    withClue("active\n") {
      active.foreach(_.isClosed shouldBe false)
    }
  }
}
