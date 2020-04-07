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
        |  ping-pong-settings {
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
        wsac.isClosed shouldBe true
        wsac.getPingsBuffer.size shouldBe 5
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
          wsac.isClosed shouldBe true
          wsac.getPingsBuffer.size should (be >= 3 and be <= 4)
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
          wsac.isClosed shouldBe true
          wsac.getPingsBuffer.size should (be >= 4 and be <= 5)
        }
      }
    }

    "even if pong is sent from another connection" in {
      val wsac1 = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)
      val wsac2 = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)

      wsac1.isClosed shouldBe false
      wsac2.isClosed shouldBe false

      Thread.sleep(pingInterval + 0.1.second)

      wsac1.getPingsBuffer should have size 1
      wsac2.getPingsBuffer should have size 1

      wsac1.sendPong(wsac2.getPingsBuffer.head) // send correct pong but from another connection
      wsac1.sendPong(wsac1.getPingsBuffer.head) // send correct pong but from another connection

      Thread.sleep(pongTimeout - 0.2.second)

      wsac1.isClosed shouldBe false
      wsac2.isClosed shouldBe false

      wsac1.getPingsBuffer should have size 3
      wsac2.getPingsBuffer should have size 3

      Thread.sleep(0.1.second)
      eventually {
        wsac1.isClosed shouldBe true
        wsac2.isClosed shouldBe true
        wsac1.getPingsBuffer.size should (be >= 3 and be <= 4)
        wsac2.getPingsBuffer.size should (be >= 3 and be <= 4)
      }
    }
  }
}
