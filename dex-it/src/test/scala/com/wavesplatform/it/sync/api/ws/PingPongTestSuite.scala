package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.websockets.HasWebSockets
import com.wavesplatform.it.MatcherSuiteBase

import scala.concurrent.duration._

class PingPongTestSuite extends MatcherSuiteBase with HasWebSockets {

  private val maxConnectionLifetime = 6.seconds
  private val pingInterval          = 1.second
  private val pongTimeout           = pingInterval * 3

  private implicit def duration2Long(d: FiniteDuration): Long = d.toMillis

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.web-sockets {
        |  max-connection-lifetime = $maxConnectionLifetime
        |  system-messages-settings {
        |    ping-interval = $pingInterval
        |    pong-timeout = $pongTimeout
        |  }
        |}
        |""".stripMargin
  )

  "Web socket connection should be closed " - {

    s"by max-connection-lifetime = $maxConnectionLifetime" in {
      val wsac = mkWsAuthenticatedConnection(alice, dex1)
      wsac.isClosed shouldBe false

      Thread.sleep(maxConnectionLifetime - 0.1.second)
      wsac.isClosed shouldBe false

      Thread.sleep(0.1.second)
      eventually {
        wsac.getPingsBuffer.size shouldBe 5
        wsac.isClosed shouldBe true
      }
    }

    s"by pong timeout (ping-interval = $pingInterval, pong-timeout = 3 * ping-interval = $pongTimeout)" - {

      "without sending pong" in {
        val wsac = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval + pongTimeout - 0.1.second)
        wsac.isClosed shouldBe false
        wsac.getPingsBuffer should have size 3

        Thread.sleep(0.1.second)
        eventually {
          wsac.getPingsBuffer.size should (be >= 3 and be <= 4)
          wsac.isClosed shouldBe true
        }
      }

      "with sending pong" in {
        val wsac = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval + 0.1.second)
        wsac.isClosed shouldBe false
        wsac.getPingsBuffer should have size 1

        wsac.sendPong(wsac.getPingsBuffer.last) // sending pong to keep connection alive

        Thread.sleep(pingInterval - 0.1.second + pongTimeout - 0.1.second)
        wsac.isClosed shouldBe false
        wsac.getPingsBuffer should have size 4

        wsac.sendPong(wsac.getPingsBuffer.tail.head) // sending outdated pong will not prolong connection lifetime

        Thread.sleep(0.1.second)
        eventually {
          wsac.getPingsBuffer.size should (be >= 4 and be <= 5)
          wsac.isClosed shouldBe true
        }
      }
    }

    "even if pong is sent from another connection" in {
      val wsac1 = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)
      val wsac2 = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)

      wsac1.isClosed shouldBe false
      wsac2.isClosed shouldBe false

      Thread.sleep(pingInterval + 0.1.second)

      Seq(wsac1, wsac2).foreach { _.getPingsBuffer should have size 1 }

      wsac1.sendPong(wsac2.getPingsBuffer.head) // send correct pong but from another connection
      wsac2.sendPong(wsac1.getPingsBuffer.head) // send correct pong but from another connection

      Thread.sleep(pongTimeout - 0.2.second)

      Seq(wsac1, wsac2).foreach { conn =>
        conn.getPingsBuffer should have size 3
        conn.isClosed shouldBe false
      }

      Thread.sleep(0.1.second)
      eventually {
        Seq(wsac1, wsac2).foreach { conn =>
          conn.getPingsBuffer.size should (be >= 3 and be <= 4)
          conn.isClosed shouldBe true
        }
      }
    }

    "by signed connection lifetime expiration, if it < max-connection-lifetime" in {
      val wsac = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false, connectionLifetime = 1.5.seconds)
      wsac.isClosed shouldBe false

      Thread.sleep(1.4.seconds)
      wsac.isClosed shouldBe false

      Thread.sleep(0.1.second)
      eventually {
        wsac.isClosed shouldBe true
        wsac.getPingsBuffer should have size 1
      }

      withClue("expiration is less than 0") {
        val wsac = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false, connectionLifetime = -1.5.hours)
        eventually {
          wsac.isClosed shouldBe true
          wsac.getPingsBuffer shouldBe empty
        }
      }
    }
  }
}
