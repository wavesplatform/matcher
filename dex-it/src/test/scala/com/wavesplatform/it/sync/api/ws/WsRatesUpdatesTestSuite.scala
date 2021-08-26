package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.it.WsSuiteBase

import scala.util.Using

class WsRatesUpdatesTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$BtcId", "$UsdId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "Matcher should send rates snapshot after subscription and rates updates" in {

    def assertRatesUpdates(wsc: WsConnection)(expectedRatesUpdates: (Asset, Double)*): Unit =
      eventually {
        val messages = wsc.collectMessages[WsRatesUpdates]
        messages.flatMap(_.rates).toMap shouldBe expectedRatesUpdates.toMap
      }

    dex1.api.upsertAssetRate(btc, 0.00041863)

    Using.resource(mkWsRatesUpdatesConnection(dex1)) { wsc1 =>

      withClue("Rates snapshot") {
        assertRatesUpdates(wsc1)(Waves -> 1, btc -> 0.00041863)
        wsc1.clearMessages()
      }

      dex1.api.upsertAssetRate(btc, 0.00041864)
      dex1.api.upsertAssetRate(usd, 2.76)

      withClue("Rates update") {
        assertRatesUpdates(wsc1)(btc -> 0.00041864, usd -> 2.76)
        wsc1.clearMessages()
      }

      Seq(btc, usd).foreach(dex1.api.deleteAssetRate)

      withClue("Rates delete") {
        assertRatesUpdates(wsc1)(
          btc -> -1,
          usd -> -1
        )
        wsc1.clearMessages()
      }

      withClue("Rates snapshot after deleting") {
        dex1.api.upsertAssetRate(btc, 0.0099)
        Using.resource(mkWsRatesUpdatesConnection(dex1)) { wsc =>
          assertRatesUpdates(wsc)(Waves -> 1, btc -> 0.0099)
        }
        dex1.api.deleteAssetRate(btc)
      }

    }
  }

  "Few subscription won't make effect" in {
    Using.resource(mkDexWsConnection(dex1)) { wsc =>
      (1 to 10).foreach { _ =>
        wsc.send(WsRatesUpdatesSubscribe())
      }

      wsc.receiveAtLeastN[WsRatesUpdates](1) should have size 1
    }
  }

  "Incorrect subscription id will cause error" in {
    Using.resource(mkDexWsConnection(dex1)) { wsc =>
      wsc.send(WsRatesUpdatesSubscribe("ur"))
      wsc.receiveAtLeastN[WsError](1)
    }
  }

  "Clients won't receive updates after unsubscribe" in {
    Using.resource(mkWsRatesUpdatesConnection(dex1)) { wsc =>
      wsc.receiveAtLeastN[WsRatesUpdates](1)

      wsc.send(WsUnsubscribe("ur"))
      wsc.receiveAtLeastN[WsError](1)

      wsc.send(WsUnsubscribe("ru"))
      Thread.sleep(1000) // No other way to guarantee that the client received and processed WsUnsubscribe

      dex1.api.upsertAssetRate(btc, 100500)
      wsc.receiveNoMessages()

      dex1.api.deleteAssetRate(btc)
    }
  }
}
