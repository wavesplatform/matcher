package com.wavesplatform.it.sync.api.ws

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.ws.TextMessage
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.error.InvalidJson
import com.wavesplatform.it.WsSuiteBase

import scala.concurrent.Await
import scala.concurrent.duration._

abstract class WsPingPongBaseSuite extends WsSuiteBase {

  protected val pingInterval: FiniteDuration = 1.second
  protected val pongTimeout: FiniteDuration = pingInterval * 3

  protected val delta: FiniteDuration = 1.second

  protected val pongTimeoutError: WsError = WsError(
    timestamp = 0L, // ignored
    code = 109077772, // WsConnectionPongTimeout
    message = "WebSocket has reached pong timeout"
  )

  implicit override def patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 30.seconds, interval = 200.millis)
  implicit protected def duration2Long(d: FiniteDuration): Long = d.toMillis

  protected def wsStreamUri: Uri
  protected def mkWsUnmanagedConnection(): WsConnection = mkWsConnection(wsStreamUri, keepAlive = false)

  "Web socket connection should be closed " - {

    s"by pong timeout (ping-interval = $pingInterval, pong-timeout = 3 * ping-interval = $pongTimeout)" - {

      "without sending pong" in {
        val wsac = mkWsUnmanagedConnection()

        val (errors, pings) = wsac.receiveAtLeastNErrorsAndPings(1, 3)

        pings.size should (be >= 3 and be <= 4)
        errors should matchTo(List(pongTimeoutError))

        val expectedConnectionLifetime = pingInterval + pongTimeout
        val connectionLifetime = Await.result(wsac.connectionLifetime, expectedConnectionLifetime + delta)

        connectionLifetime should (be >= expectedConnectionLifetime and be <= expectedConnectionLifetime + delta)
        wsac.isClosed shouldBe true
      }

      "with sending pong" in {
        val wsac = mkWsUnmanagedConnection()

        val firstPingList = wsac.receiveAtLeastN[WsPingOrPong](1)
        firstPingList should have size 1
        wsac.clearMessages()

        val firstPing = firstPingList.head
        wsac.send(firstPing) // sending pong to keep connection alive

        val secondPingList = wsac.receiveAtLeastN[WsPingOrPong](1)
        secondPingList should have size 1
        wsac.clearMessages()

        wsac.send(firstPing) // sending outdated pong will not prolong connection lifetime

        val (errors, pings) = wsac.receiveAtLeastNErrorsAndPings(1, 2)

        pings.size should (be >= 2 and be <= 3)
        errors should matchTo(List(pongTimeoutError))

        val connectionLifetime = Await.result(wsac.connectionLifetime, pongTimeout + delta)
        val expectedConnectionLifetime = pingInterval * 2 + pongTimeout

        connectionLifetime should (be >= expectedConnectionLifetime and be <= expectedConnectionLifetime + delta)
        wsac.isClosed shouldBe true
      }

      "even if pong is sent from another connection" in {
        val wsac1 = mkWsUnmanagedConnection()
        val wsac2 = mkWsUnmanagedConnection()

        Seq(wsac1, wsac2).foreach(_.receiveAtLeastN[WsPingOrPong](1) should have size 1) // first ping received

        wsac1.send(wsac2.pings.head) // send correct pong but from another connection
        wsac2.send(wsac1.pings.head) // send correct pong but from another connection

        Seq(wsac1, wsac2).foreach(_.clearMessages())

        val expectedConnectionsLifetime = pingInterval + pongTimeout
        val connection1Lifetime = Await.result(wsac1.connectionLifetime, pongTimeout + delta)
        val connection2Lifetime = Await.result(wsac2.connectionLifetime, pongTimeout + delta)

        Seq(wsac1 -> connection1Lifetime, wsac2 -> connection2Lifetime).foreach {
          case (conn, connLifetime) =>
            withClue(s"testId=${conn.testId}") {
              val (errors, pings) = conn.receiveAtLeastNErrorsAndPings(1, 2)
              connLifetime should (be >= expectedConnectionsLifetime and be <= expectedConnectionsLifetime + delta)
              pings.size should (be >= 2 and be <= 3)
              errors should matchTo(List(pongTimeoutError))
              conn.isClosed shouldBe true
            }
        }
      }
    }
  }

  "Web socket connection should not be closed " - {

    "when incorrect message has been sent to the Matcher" in {

      val wsc = new WsConnection(wsStreamUri, keepAlive = false) {
        override def stringifyClientMessage(cm: WsClientMessage): TextMessage.Strict = cm match {
          case WsPingOrPong(timestamp) if timestamp == -1 => TextMessage.Strict(s"broken")
          case other: WsClientMessage => WsMessage.toStrictTextMessage(other)(WsClientMessage.wsClientMessageWrites)
        }
      }

      val firstPingList = wsc.receiveAtLeastN[WsPingOrPong](1) // first ping received
      firstPingList should have size 1
      val firstPing = firstPingList.head

      wsc.send(firstPing) // first pong sent
      wsc.send(firstPing.copy(timestamp = -1)) // wrong message sent
      wsc.clearMessages()

      val connectionLifetime = Await.result(wsc.connectionLifetime, pingInterval + pongTimeout + delta)
      val expectedConnectionsLifetime = pingInterval * 2 + pongTimeout
      val (errors, pings) = wsc.receiveAtLeastNErrorsAndPings(1, 3)
      val expectedError = InvalidJson(Nil)

      connectionLifetime should (be >= expectedConnectionsLifetime and be <= expectedConnectionsLifetime + delta)

      pings.size should (be >= 3 and be <= 4)

      errors.size should be(2)
      val List(actualInvalidJsonError, actualPongTimeoutError) = errors

      actualInvalidJsonError.code should be(expectedError.code)
      actualInvalidJsonError.message should include regex "broken"

      actualPongTimeoutError should matchTo(pongTimeoutError)

      wsc.isClosed shouldBe true
    }
  }
}
