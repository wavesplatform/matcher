package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.it.WsSuiteBase

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

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
