package com.wavesplatform.it.sync.api.ws

import akka.http.scaladsl.model.ws.TextMessage
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.connection.WsConnection
import com.wavesplatform.dex.api.websockets.{WsClientMessage, WsError, WsMessage, WsPingOrPong}
import com.wavesplatform.dex.error.InvalidJson
import com.wavesplatform.it.WsSuiteBase

import scala.concurrent.duration._

class WsPingPongTestSuite extends WsSuiteBase {

  private val maxConnectionLifetime = 6.seconds
  private val pingInterval          = 1.second
  private val pongTimeout           = pingInterval * 3

  private implicit def duration2Long(d: FiniteDuration): Long = d.toMillis

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex.web-sockets.web-socket-handler {
        |    max-connection-lifetime = $maxConnectionLifetime
        |    ping-interval = $pingInterval
        |    pong-timeout = $pongTimeout
        | }
        |""".stripMargin
    )
    .withFallback(jwtPublicKeyConfig)

  "Web socket connection should be closed " - {

    s"by max-connection-lifetime = $maxConnectionLifetime" in {
      val wsac = mkWsAddressConnection(alice, dex1)
      wsac.isClosed shouldBe false

      Thread.sleep(maxConnectionLifetime - 0.2.second)
      wsac.isClosed shouldBe false

      Thread.sleep(0.2.second)
      eventually {
        wsac.pings.size should be >= 5
        wsac.isClosed shouldBe true
      }

      wsac.collectMessages[WsError].head should matchTo(
        WsError(
          timestamp = 0L, // ignored
          code = 109077767, // WsConnectionMaxLifetimeExceeded
          message = "WebSocket has reached max allowed lifetime"
        )
      )
    }

    s"by pong timeout (ping-interval = $pingInterval, pong-timeout = 3 * ping-interval = $pongTimeout)" - {

      "without sending pong" in {
        val wsac = mkWsAddressConnection(alice, dex1, keepAlive = false)
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval + pongTimeout - 0.1.second)
        wsac.isClosed shouldBe false
        wsac.pings should have size 3

        Thread.sleep(0.1.second)
        eventually {
          wsac.pings.size should (be >= 3 and be <= 4)
          wsac.isClosed shouldBe true
        }

        wsac.collectMessages[WsError].head should matchTo(
          WsError(
            timestamp = 0L, // ignored
            code = 109077772, // WsConnectionPongTimeout
            message = "WebSocket has reached pong timeout"
          )
        )
      }

      "with sending pong" in {
        val wsac = mkWsAddressConnection(alice, dex1, keepAlive = false)
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval + 0.1.second)
        wsac.isClosed shouldBe false
        wsac.pings should have size 1

        wsac.send(wsac.pings.last) // sending pong to keep connection alive

        Thread.sleep(pingInterval - 0.1.second + pongTimeout - 0.1.second)
        wsac.isClosed shouldBe false
        wsac.pings should have size 4

        wsac.send(wsac.pings.tail.head) // sending outdated pong will not prolong connection lifetime

        Thread.sleep(0.1.second)
        eventually {
          wsac.pings.size should (be >= 4 and be <= 5)
          wsac.isClosed shouldBe true
        }
      }
    }

    "even if pong is sent from another connection" in {
      val wsac1 = mkWsAddressConnection(alice, dex1, keepAlive = false)
      val wsac2 = mkWsAddressConnection(alice, dex1, keepAlive = false)

      wsac1.isClosed shouldBe false
      wsac2.isClosed shouldBe false

      Thread.sleep(pingInterval + 0.1.second)

      Seq(wsac1, wsac2).foreach { _.pings should have size 1 }

      wsac1.send(wsac2.pings.head) // send correct pong but from another connection
      wsac2.send(wsac1.pings.head) // send correct pong but from another connection

      Thread.sleep(pongTimeout - 0.3.second)

      Seq(wsac1, wsac2).foreach { conn =>
        conn.pings should have size 3
        conn.isClosed shouldBe false
      }

      Thread.sleep(0.3.second)
      eventually {
        Seq(wsac1, wsac2).foreach { conn =>
          conn.pings.size should (be >= 3 and be <= 4)
          conn.isClosed shouldBe true
        }
      }
    }
  }

  "Web socket connection should not be closed " - {

    "when incorrect message has been sent" in {

      val wsc = new WsConnection(getWsStreamUri(dex1), keepAlive = false) {
        override def stringifyClientMessage(cm: WsClientMessage): TextMessage.Strict = cm match {
          case WsPingOrPong(timestamp) if timestamp == 0 => TextMessage.Strict(s"broken")
          case other: WsClientMessage                    => WsMessage.toStrictTextMessage(other)(WsClientMessage.wsClientMessageWrites)
        }
      }

      Thread.sleep(pingInterval + 0.1.second)
      wsc.isClosed shouldBe false
      wsc.pings should have size 1

      wsc.send(wsc.pings.last)
      wsc.send(wsc.pings.last.copy(timestamp = 0))

      Thread.sleep(pingInterval - 0.1.second + pongTimeout - 0.1.second)
      wsc.pings should have size 4

      val expectedError = InvalidJson(Nil)
      wsc.collectMessages[WsError] should matchTo { List(WsError(0L, expectedError.code, expectedError.message.text)) }

      wsc.isClosed shouldBe false
    }
  }
}
