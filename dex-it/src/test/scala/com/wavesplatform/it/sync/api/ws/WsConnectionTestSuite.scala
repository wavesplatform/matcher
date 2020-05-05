package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsAddressSubscribe, WsOrderBook, WsOrderBookSubscribe, WsUnsubscribe}
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.it.WsSuiteBase

class WsConnectionTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex.price-assets = [ "$BtcId", "WAVES" ]""")
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx)
    dex1.start()
  }

  "Updates both from address and order book" in {
    val wsc = mkWsConnection(dex1)

    markup("Subscribe to an order book updates")
    wsc.send(WsOrderBookSubscribe(wavesBtcPair, 1))
    wsc.receiveAtLeastN[WsOrderBook](1)
    wsc.clearMessages()

    markup("Subscribe to an address updates")
    wsc.send(WsAddressSubscribe(alice, WsAddressSubscribe.defaultAuthType, mkJwt(alice)))
    wsc.receiveAtLeastN[WsAddressState](1)
    wsc.clearMessages()

    markup("Place an order")
    val order = mkOrderDP(alice, wavesBtcPair, SELL, 1.waves, 0.00005)
    placeAndAwaitAtDex(order)
    wsc.receiveAtLeastN[WsOrderBook](1)
    wsc.receiveAtLeastN[WsAddressState](1)
    wsc.clearMessages()

    markup("Unsubscribe from an address updates")
    wsc.send(WsUnsubscribe(alice))

    markup("Cancel an order")
    cancelAndAwait(alice, order)
    wsc.receiveAtLeastN[WsOrderBook](1)
    wsc.receiveNoMessagesOf[WsAddressState]()

    wsc.close()
  }
}
