package com.wavesplatform.it.sync.api.ws

import akka.http.scaladsl.model.Uri
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.Implicits.durationToScalatestTimeout
import com.wavesplatform.dex.api.ws.protocol.WsError
import com.wavesplatform.dex.error.WsConnectionMaxLifetimeExceeded

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Using

class WsPingPongExternalTestSuite extends WsPingPongBaseSuite {

  protected val maxConnectionLifetime: FiniteDuration = 6.seconds

  override protected lazy val wsStreamUri: Uri = getWsStreamUri(dex1)

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex.web-sockets.external-client-handler {
         |    max-connection-lifetime = $maxConnectionLifetime
         |    health-check = {
         |      ping-interval = $pingInterval
         |      pong-timeout = $pongTimeout
         |    }
         | }
         |""".stripMargin
    )
    .withFallback(jwtPublicKeyConfig)

  "Web socket connection should be closed " - {
    s"by max-connection-lifetime = $maxConnectionLifetime" in {

      Using.resource(mkWsAddressConnection(alice, dex1)) { wsac =>
        val connectionLifetime = wsac.connectionLifetime.futureValue(maxConnectionLifetime + delta)
        val (errors, pings) = wsac.receiveAtLeastNErrorsAndPings(1, 5)

        connectionLifetime should (be >= maxConnectionLifetime and be <= maxConnectionLifetime + delta)

        pings.size should (be >= 5 and be <= 6)

        errors should matchTo {
          List(
            WsError(
              timestamp = 0L, // ignored
              code = WsConnectionMaxLifetimeExceeded.code,
              message = "WebSocket has reached max allowed lifetime"
            )
          )
        }

        wsac.isClosed shouldBe true
      }

    }
  }
}
