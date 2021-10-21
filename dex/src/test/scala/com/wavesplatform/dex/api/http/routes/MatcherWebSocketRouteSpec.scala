package com.wavesplatform.dex.api.http.routes

import akka.actor.ActorRef
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex._
import com.wavesplatform.dex.api.RouteSpec
import com.wavesplatform.dex.api.http.ApiMarshallers._
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.http.headers.`X-Api-Key`
import com.wavesplatform.dex.api.ws.actors.{WsExternalClientDirectoryActor, WsInternalBroadcastActor}
import com.wavesplatform.dex.api.ws.routes.MatcherWebSocketRoute
import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.effect._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.time.TestTime
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsonFacade => _}
import pureconfig.ConfigSource

import scala.annotation.nowarn
import scala.util.Random

class MatcherWebSocketRouteSpec extends RouteSpec("/ws/v0") with MatcherSpecBase with PathMockFactory with Eventually {

  private val testKit = ActorTestKit()

  private val apiKey = "apiKey"
  private val apiKeyHeader = RawHeader(`X-Api-Key`.headerName, apiKey)

  private val settings = ConfigSource.fromConfig(ConfigFactory.load()).at("waves.dex").loadOrThrow[MatcherSettings].copy(priceAssets = Seq(Waves))

  routePath("/connections") - {
    "connectionsRoute" - {
      "returns connections info" in test(
        route =>
          Get(routePath("/connections")).withHeaders(apiKeyHeader) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            responseAs[HttpWebSocketConnections] should matchTo(HttpWebSocketConnections(0, Map.empty))
          },
        apiKey
      )
    }

    "closeConnectionsRoute" - {
      "returns a closed connections info" in test(
        route =>
          Delete(routePath("/connections"), HttpWebSocketCloseFilter(100)).withHeaders(apiKeyHeader) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            responseAs[HttpMessage] should matchTo(HttpMessage("In progress"))
          },
        apiKey
      )
    }
  }

  @nowarn("msg=default")
  private def test[U](f: Route => U, apiKey: String = ""): U = {
    val time = new TestTime
    val vsInternalBroadcastRef = testKit.createTestProbe[WsInternalBroadcastActor.Command]()
    val route =
      new MatcherWebSocketRoute(
        wsInternalBroadcastRef = vsInternalBroadcastRef.ref,
        externalClientDirectoryRef = testKit.spawn(WsExternalClientDirectoryActor(), s"ws-external-cd-${Random.nextInt(Int.MaxValue)}"),
        addressDirectory = ActorRef.noSender,
        matcher = ActorRef.noSender,
        time = time,
        assetPairBuilder = new AssetPairBuilder(
          settings,
          x => liftErrorAsync[BriefAssetDescription](error.AssetNotFound(x)),
          Set.empty
        ),
        apiKeyHashes = List(crypto secureHash apiKey),
        matcherSettings = settings,
        matcherStatus = () => MatcherStatus.Working,
        () => rateCache.getAllRates
      )

    f(route.route)
  }

}
