package com.wavesplatform.it.sync.api.ws

import akka.http.scaladsl.model.ws.TextMessage
import com.wavesplatform.dex.api.websockets.connection.WsConnection
import com.wavesplatform.dex.api.websockets.{WsClientMessage, WsError, WsMessage, WsPingOrPong}
import com.wavesplatform.dex.error.InvalidJson
import com.wavesplatform.it.WsSuiteBase

import scala.concurrent.Await
import scala.concurrent.duration._

abstract class WsPingPongBaseSuite extends WsSuiteBase {

  protected val pingInterval = 1.second
  protected val pongTimeout  = pingInterval * 3

  protected val delta = 1.second

  private implicit def duration2Long(d: FiniteDuration): Long = d.toMillis

  protected def wsStreamUri: String
  protected def mkWsTestConnection(): WsConnection = mkWsConnection(wsStreamUri, keepAlive = false)

  "Web socket connection should be closed " - {

    s"by pong timeout (ping-interval = $pingInterval, pong-timeout = 3 * ping-interval = $pongTimeout)" - {

      "without sending pong" in {
        val wsac                       = mkWsTestConnection()
        val expectedConnectionLifetime = pingInterval + pongTimeout
        val connectionLifetime         = Await.result(wsac.connectionLifetime, expectedConnectionLifetime + delta)

        connectionLifetime should (be >= expectedConnectionLifetime and be <= expectedConnectionLifetime + delta)
        wsac.pings.size should (be >= 3 and be <= 4)
        wsac.isClosed shouldBe true

        wsac.collectMessages[WsError].head should matchTo(
          WsError(
            timestamp = 0L, // ignored
            code = 109077772, // WsConnectionPongTimeout
            message = "WebSocket has reached pong timeout"
          )
        )
      }

      "with sending pong" in {
        val wsac = mkWsTestConnection()

        Thread.sleep(pingInterval + 0.1.second)
        wsac.isClosed shouldBe false
        wsac.pings should have size 1

        wsac.send(wsac.pings.last) // sending pong to keep connection alive

        Thread.sleep(pingInterval + 0.1.second)
        wsac.isClosed shouldBe false
        wsac.pings should have size 2

        wsac.send(wsac.pings.head) // sending outdated pong will not prolong connection lifetime

        val connectionLifetime         = Await.result(wsac.connectionLifetime, pongTimeout + delta)
        val expectedConnectionLifetime = pingInterval * 2 + pongTimeout

        connectionLifetime should (be >= expectedConnectionLifetime and be <= expectedConnectionLifetime + delta)
        wsac.pings.size should (be >= 4 and be <= 5)
        wsac.isClosed shouldBe true
      }

      "even if pong is sent from another connection" in {
        val wsac1 = mkWsTestConnection()
        val wsac2 = mkWsTestConnection()

        wsac1.isClosed shouldBe false
        wsac2.isClosed shouldBe false

        Thread.sleep(pingInterval + 0.1.second)

        Seq(wsac1, wsac2).foreach {
          _.pings should have size 1
        }

        wsac1.send(wsac2.pings.head) // send correct pong but from another connection
        wsac2.send(wsac1.pings.head) // send correct pong but from another connection

        val expectedConnectionsLifetime = pingInterval + pongTimeout
        val connection1Lifetime         = Await.result(wsac1.connectionLifetime, pongTimeout + delta)
        val connection2Lifetime         = Await.result(wsac2.connectionLifetime, pongTimeout + delta)

        Seq(wsac1 -> connection1Lifetime, wsac2 -> connection2Lifetime).foreach {
          case (conn, connLifetime) =>
            connLifetime should (be >= expectedConnectionsLifetime and be <= expectedConnectionsLifetime + delta)
            conn.pings.size should (be >= 3 and be <= 4)
            conn.isClosed shouldBe true
        }
      }
    }
  }

  "Web socket connection should not be closed " - {

    "when incorrect message has been sent to te Matcher" in {
      val wsc = new WsConnection(wsStreamUri, keepAlive = false) {
        override def stringifyClientMessage(cm: WsClientMessage): TextMessage.Strict = cm match {
          case WsPingOrPong(timestamp) if timestamp == -1 => TextMessage.Strict(s"broken")
          case other: WsClientMessage                     => WsMessage.toStrictTextMessage(other)(WsClientMessage.wsClientMessageWrites)
        }
      }

      Thread.sleep(pingInterval + 0.1.second)
      wsc.isClosed shouldBe false
      wsc.pings should have size 1

      wsc.send(wsc.pings.last)
      wsc.send(wsc.pings.last.copy(timestamp = -1))

      val connectionLifetime          = Await.result(wsc.connectionLifetime, pingInterval + pongTimeout + delta)
      val expectedConnectionsLifetime = pingInterval * 2 + pongTimeout
      connectionLifetime should (be >= expectedConnectionsLifetime and be <= expectedConnectionsLifetime + delta)

      wsc.pings should have size 4
      wsc.isClosed shouldBe true

      val expectedError = InvalidJson(Nil)

      wsc.collectMessages[WsError] should matchTo {
        List(
          WsError(0L, expectedError.code, expectedError.message.text),
          WsError(
            timestamp = 0L, // ignored
            code = 109077772, // WsConnectionPongTimeout
            message = "WebSocket has reached pong timeout"
          )
        )
      }
    }
  }
}
