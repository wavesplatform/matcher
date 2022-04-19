package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsLastTrade}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsOrderBookChanges, WsOrderBookSubscribe}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.WsSuiteBase

import scala.util.Using

class WsZeroUpdatesTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdnId", "WAVES" ]
       |  web-sockets.external-client-handler.messages-interval = 2s
       |}""".stripMargin
  ).withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdnTx)
    dex1.start()
  }

  "WsZeroUpdates" - {
    "receive complete update after filling its own order" in {
      Using.resource(mkWsAddressConnection(alice, dex1)) { wsc =>
        wsc.send(
          WsOrderBookSubscribe(
            wavesUsdnPair,
            1
          )
        )
        val initStateOb = eventually(wsc.receiveAtLeastN[WsOrderBookChanges](1)).head
        val initStateAc = eventually(wsc.receiveAtLeastN[WsAddressChanges](1)).head

        wsc.clearMessages()

        val order1 = mkOrder(alice, wavesUsdnPair, OrderType.BUY, 50.waves, 20.usdn)
        val order2 = mkOrder(alice, wavesUsdnPair, OrderType.SELL, 50.waves, 20.usdn)
        dex1.api.place(order1)
        dex1.api.place(order2)
        waitForOrderAtNode(order2)

        wsc.receiveAtLeastN[WsOrderBookChanges](1)
        wsc.receiveAtLeastN[WsAddressChanges](1)
        val balances = wsc.addressStateChanges.foldLeft(Map.empty[Asset, WsBalances]) {
          case (acc, change) =>
            (acc ++ change.balances).filter {
              case (asset, balance) => !initStateAc.balances.get(asset).contains(balance)
            }
        }
        val obChanges = wsc.collectMessages[WsOrderBookChanges].foldLeft(WsOrderBookChanges.empty(wavesUsdnPair)) {
          case (acc, change) =>
            acc.copy(
              asks = change.asks.filter(lvl => lvl._2 != 0),
              bids = change.bids.filter(lvl => lvl._2 != 0),
              lastTrade = acc.lastTrade.orElse(change.lastTrade)
            )
        }
        balances.keySet shouldBe Set(Waves)
        balances(Waves).tradable shouldBe (4_949_950d - 0.003 * 2 - 1) // two orders + issue fee 1.waves
        obChanges.asks.isEmpty shouldBe true
        obChanges.bids.isEmpty shouldBe true
        obChanges.lastTrade.value shouldBe WsLastTrade(20.0, 50.0, OrderType.SELL)
        wsc.receiveNoMessages()
      }
    }
  }

}
