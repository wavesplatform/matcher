package com.wavesplatform.it.sync.api.ws

import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpWebSocketConnections
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.fp.MapImplicits.MapNumericOps
import com.wavesplatform.dex.it.docker.DexContainer
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

  "getConnections returns the right statistics" in {
    def expectClientAndOs(info: HttpWebSocketConnections,
                          firefoxLinux: Int,
                          unknownOs2: Int,
                          androidUnknown: Int,
                          unknownUnknown: Int,
                          additional: (String, Int)*): Unit =
      info.clientAndOs should matchTo {
        Map
          .empty[String, Int]
          .appendIfNonZeroMany(
            Seq(
              "Firefox, Linux 5.2"         -> firefoxLinux,
              "Unknown Client, OS/2"       -> unknownOs2,
              "Android 10, Unknown OS"     -> androidUnknown,
              "Unknown Client, Unknown OS" -> unknownUnknown
            ) ++ additional: _*
          )
      }

    val firefoxLinuxWscs = mkDexWsConnections(1, os = "Linux 5.2".some, client = "Firefox".some)
    val unknownOs2Wscs   = mkDexWsConnections(2, os = "OS/2".some)

    val wscs = firefoxLinuxWscs ++
      unknownOs2Wscs ++
      mkDexWsConnections(3, client = "Android 10".some) ++
      mkDexWsConnections(4)

    expectClientAndOs(dex1.api.waitForWsConnections(_.connections == 10), 1, 2, 3, 4)

    info("Closing all Firefox + Linux 5.2 and one Unknown + OS/2")
    firefoxLinuxWscs.foreach(_.close())
    unknownOs2Wscs.head.close()
    expectClientAndOs(dex1.api.waitForWsConnections(_.connections == 8), 0, 1, 3, 4)

    info("Opening a new client")
    val newWs = mkDexWsConnection(dex1, os = "Test OS".some, client = "Test Client".some)
    expectClientAndOs(dex1.api.waitForWsConnections(_.connections == 9), 0, 1, 3, 4, "Test Client, Test OS" -> 1)

    newWs.close()
    wscs.foreach(_.close())
  }

  "closeConnection closes N oldest connections" in {
    val wscs = mkDexWsConnections(10)
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

  private def mkDexWsConnections(n: Int, dex: DexContainer = dex1, os: Option[String] = None, client: Option[String] = None): Seq[WsConnection] =
    (1 to n).map(_ => mkDexWsConnection(dex, os = os, client = client))
}
