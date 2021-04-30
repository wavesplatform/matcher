package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.it.WsSuiteBase

class WsRatesUpdatesTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$BtcId", "$UsdId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "Matcher should send rates snapshot after subscription and rates updates" in {

    def assertRatesUpdates(wsc: WsConnection)(expectedRatesUpdates: List[(Map[Asset, Double], Long)]): Unit =
      wsc
        .receiveAtLeastN[WsRatesUpdates](expectedRatesUpdates.size)
        .map(r => r.rates -> r.updateId) should matchTo(expectedRatesUpdates)

    dex1.api.upsertRate(btc, 0.00041863)

    val wsc1 = mkWsRatesUpdatesConnection(dex1)

    withClue("Rates snapshot") {
      assertRatesUpdates(wsc1)(List((Map(Waves -> 1, btc -> 0.00041863), 0)))
      wsc1.clearMessages()
    }

    dex1.api.upsertRate(btc, 0.00041864)

    val wsc2 = mkWsRatesUpdatesConnection(dex1)

    dex1.api.upsertRate(usd, 2.76)

    withClue("Rates update") {
      assertRatesUpdates(wsc1) {
        List(
          Map[Asset, Double](btc -> 0.00041864) -> 1L,
          Map[Asset, Double](usd -> 2.76) -> 2L
        )
      }
      assertRatesUpdates(wsc2) {
        List(
          Map[Asset, Double](Waves -> 1, btc -> 0.00041864) -> 0L,
          Map[Asset, Double](usd -> 2.76) -> 1L
        )
      }
      Seq(wsc1, wsc2).foreach(_.clearMessages())
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
      Seq(wsc1, wsc2).foreach(_.clearMessages())
    }

    withClue("Rates snapshot after deleting") {
      dex1.api.upsertRate(btc, 0.0099)
      val wsc = mkWsRatesUpdatesConnection(dex1)
      assertRatesUpdates(wsc)(List(Map[Asset, Double](Waves -> 1, btc -> 0.0099) -> 0))
      wsc.close()
      dex1.api.deleteRate(btc)
    }

    Seq(wsc1, wsc2).foreach(_.close())
  }

  "Few subscription won't make effect" in {
    val wsc = mkDexWsConnection(dex1)

    (1 to 10).foreach { _ =>
      wsc.send(WsRatesUpdatesSubscribe())
    }

    wsc.receiveAtLeastN[WsRatesUpdates](1) should have size 1
  }

  "Incorrect subscription id will cause error" in {
    val wsc = mkDexWsConnection(dex1)
    wsc.send(WsRatesUpdatesSubscribe("ur"))
    wsc.receiveAtLeastN[WsError](1)
  }

  "Clients won't receive updates after unsubscribe" in {
    val wsc = mkWsRatesUpdatesConnection(dex1)

    wsc.receiveAtLeastN[WsRatesUpdates](1)

    wsc.send(WsUnsubscribe("ur"))
    wsc.receiveAtLeastN[WsError](1)

    wsc.send(WsUnsubscribe("ru"))
    Thread.sleep(1000) // No other way to guarantee that the client received and processed WsUnsubscribe

    dex1.api.upsertRate(btc, 100500)
    wsc.receiveNoMessages()

    dex1.api.deleteRate(btc)
    wsc.close()
  }
}
