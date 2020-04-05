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
      eventually { wsac.isClosed shouldBe true }
    }

    s"by pong timeout (ping-interval = $pingInterval, pong-timeout = 3 * ping-interval = $pongTimeout)" - {

      "without sending pong" in {
        val wsac = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval * 3) // in 4 ping-interval will be closed
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval)
        eventually { wsac.isClosed shouldBe true }
      }

      "with sending pong" in {
        val wsac = mkWsAuthenticatedConnection(alice, dex1, keepAlive = false)
        wsac.isClosed shouldBe false

        Thread.sleep(pingInterval + 0.1.second)
        wsac.isClosed shouldBe false

        wsac.sendPong()

        Thread.sleep(pongTimeout - 0.2.second)
        wsac.isClosed shouldBe false

        Thread.sleep(0.1.second)
        eventually { wsac.isClosed shouldBe true }
      }
    }
  }
}
