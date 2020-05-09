package com.wavesplatform.dex.api.websockets

import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe => TypedTestProbe}
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter._
import akka.testkit.TestProbe
import cats.syntax.either._
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex._
import com.wavesplatform.dex.api.websockets.actors.WsHandlerActor
import com.wavesplatform.dex.api.websockets.actors.WsHandlerActor.Command.ProcessClientMessage
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.market.AggregatedOrderBookActor
import com.wavesplatform.dex.market.MatcherActor.AggregatedOrderBookEnvelope
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import scala.concurrent.duration._

class WsHandlerActorSpec extends AnyFreeSpecLike with Matchers with MatcherSpecBase with HasJwt {

  private val testKit = ActorTestKit()

  private implicit val ec                         = testKit.system.executionContext
  private implicit val wsErrorDiff: Diff[WsError] = Derived[Diff[WsError]].ignore[Long](_.timestamp)

  private val issuedAsset   = IssuedAsset(ByteStr("issuedAsset".getBytes(StandardCharsets.UTF_8)))
  private val assetPair     = AssetPair(issuedAsset, Waves)
  private val clientKeyPair = mkKeyPair("seed")

  "WsHandlerActor" - {
    "WsOrderBookSubscribe" - {
      "sunny day" in test { t =>
        t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(assetPair, 1))
        t.matcherProbe.expectMsg(
          AggregatedOrderBookEnvelope(
            assetPair,
            AggregatedOrderBookActor.Command.AddWsSubscription(t.clientProbe.ref)
          )
        )
      }

      "AssetPairBuilder validation" in {
        val invalidAssetPair = AssetPair(Waves, issuedAsset)
        failTest(
          WsOrderBookSubscribe(invalidAssetPair, 1),
          WsError(
            timestamp = 0L, // ignored
            code = 9440771, // OrderAssetPairReversed
            message = "The WAVES-T9euE6z4DWHFWxT asset pair should be reversed"
          )
        )
      }

      "negative depth" in failTest(
        WsOrderBookSubscribe(assetPair, -1),
        WsError(
          timestamp = 0L, // ignored
          code = 1048576, // RequestArgumentInvalid
          message = "The request argument 'depth' is invalid"
        )
      )
    }

    "WsAddressSubscribe" - {
      "sunny day" in test { t =>
        val jwtPayload = mkJwtSignedPayload(clientKeyPair)
        t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, mkJwt(jwtPayload)))
        t.addressProbe.expectMsg(
          AddressDirectory.Envelope(
            clientKeyPair,
            AddressActor.WsCommand.AddWsSubscription(t.clientProbe.ref)
          )
        )
      }

      "jwt is invalid" - {
        "wrong auth type" in failTest(
          WsAddressSubscribe(clientKeyPair, "password", ""),
          WsError(
            timestamp = 0L, // ignored
            code = 106960131, // SubscriptionAuthTypeUnsupported
            message = "The subscription authentication type 'password' is not supported. Required one of: jwt"
          )
        )

        "broken" in failTest(
          "dGVzdA==",
          WsError(
            timestamp = 0L, // ignored
            code = 110100481, // JwtBroken
            message = "JWT has invalid format"
          )
        )

        "expired" in failTest(
          mkJwt(mkJwtSignedPayload(clientKeyPair, lifetime = -1.minute)),
          WsError(
            timestamp = 0L, // ignored
            code = 110105088, // SubscriptionTokenExpired
            message = s"The subscription token for address ${clientKeyPair.toAddress} expired"
          )
        )

        "parsing or validation issues" in failTest(
          "dGVzdA==.dGVzdA==",
          WsError(
            timestamp = 0L, // ignored
            code = 110100480, // JwtCommonError
            message =
              "JWT parsing and validation failed: Unrecognized token 'test': was expecting (JSON String, Number, Array, Object or token 'null', 'true' or 'false')  at [Source: (String)\"test\"; line: 1, column: 9]"
          )
        )

        "wrong payload" in {
          val jwtPayload = Json.toJsObject(mkJwtSignedPayload(clientKeyPair)) - "scope"
          failTest(
            mkJwt(jwtPayload),
            WsError(
              timestamp = 0L, // ignored
              code = 110127617, // JwtPayloadBroken
              message = "JWT payload has not expected fields"
            )
          )
        }

        "wrong network" in failTest(
          mkJwt(mkJwtSignedPayload(clientKeyPair, networkByte = 0)),
          WsError(
            timestamp = 0L, // ignored
            code = 110106116, // TokenNetworkUnexpected
            message = "The required network is 84, but given 0"
          )
        )

        "invalid signature" in failTest(
          mkJwt(mkJwtNotSignedPayload(clientKeyPair)),
          WsError(
            timestamp = 0L, // ignored
            code = 110103809, // InvalidJwtPayloadSignature
            message = "The token payload signature is invalid"
          )
        )

        "address doesn't fit public key" in failTest(
          WsAddressSubscribe(KeyPair(ByteStr("other-client".getBytes(StandardCharsets.UTF_8))), "jwt", mkJwt(mkJwtSignedPayload(clientKeyPair))),
          WsError(
            timestamp = 0L, // ignored
            code = 106957828, // AddressAndPublicKeyAreIncompatible
            message = "Address 3N7nTwcKubzsH6X1uWerLoYFGX1pTKSbhUu and public key D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5 are incompatible"
          )
        )
      }

      "should be cancelled after jwt expiration" in test { t =>
        val jwtPayload = mkJwtSignedPayload(clientKeyPair, lifetime = 1.second)

        t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, mkJwt(jwtPayload)))
        val error = t.clientProbe.expectMessageType[WsError]

        error.code shouldBe 110105088 // SubscriptionTokenExpired
        error.message shouldBe s"The subscription token for address ${clientKeyPair.toAddress} expired"
      }

      "should be extended after jwt update" in test { t =>
        def requestSubscriptionWithLifetime(lifetime: FiniteDuration): Unit = {
          val jwtPayload = mkJwtSignedPayload(clientKeyPair, lifetime = lifetime)
          t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, mkJwt(jwtPayload)))
        }

        requestSubscriptionWithLifetime(2.seconds)
        requestSubscriptionWithLifetime(1.hour)

        t.clientProbe.expectNoMessage(2.1.seconds)
      }
    }

    "should close all subscriptions after connection closing" in test { t =>
      import AddressActor.WsCommand
      import AggregatedOrderBookActor.Command

      val jwtPayload = mkJwtSignedPayload(clientKeyPair)
      val clientRef  = t.clientProbe.ref

      t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(assetPair, 1))
      t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, mkJwt(jwtPayload)))

      t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(assetPair, Command.AddWsSubscription(clientRef)))
      t.addressProbe.expectMsg(AddressDirectory.Envelope(clientKeyPair, WsCommand.AddWsSubscription(clientRef)))

      t.wsHandlerRef ! WsHandlerActor.Completed(().asRight)

      t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(assetPair, Command.RemoveWsSubscription(clientRef)))
      t.addressProbe.expectMsg(AddressDirectory.Envelope(clientKeyPair, WsCommand.RemoveWsSubscription(clientRef)))
    }
  }

  private def failTest(jwt: String, expectedError: WsError): Unit = failTest(
    WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, jwt),
    expectedError
  )

  private def failTest(subscribe: WsOrderBookSubscribe, expectedError: WsError): Unit =
    failTest(ProcessClientMessage(subscribe), expectedError)

  private def failTest(subscribe: WsAddressSubscribe, expectedError: WsError): Unit =
    failTest(ProcessClientMessage(subscribe), expectedError)

  private def failTest(message: ProcessClientMessage, expectedError: WsError): Unit = test { t =>
    t.wsHandlerRef ! message
    t.clientProbe.receiveMessage() match {
      case actualError: WsError => actualError should matchTo(expectedError)
      case x                    => fail(s"Unexpected message: $x")
    }
  }

  private case class TestInstances(clientProbe: TypedTestProbe[WsMessage],
                                   matcherProbe: TestProbe,
                                   addressProbe: TestProbe,
                                   wsHandlerRef: ActorRef[WsHandlerActor.Message])

  private def test(f: TestInstances => Unit): Unit = {
    val clientInbox  = TypedTestProbe[WsMessage]()(testKit.system)
    val matcherProbe = TestProbe(UUID.randomUUID().toString)(testKit.system.toClassic)
    val addressProbe = TestProbe(UUID.randomUUID().toString)(testKit.system.toClassic)

    val wsHandlerRef = testKit.spawn(
      WsHandlerActor(
        settings = WsHandlerActor.Settings(10.minutes, 1.minute, 3.minutes, Base64.getEncoder.encodeToString(authServiceKeyPair.getPublic.getEncoded)),
        time = time,
        assetPairBuilder = new AssetPairBuilder(
          matcherSettings,
          assetDescription = x => effect.liftValueAsync(BriefAssetDescription(x.toString, 8, hasScript = false)),
          blacklistedAssets = Set.empty
        ),
        clientRef = clientInbox.ref,
        matcherRef = matcherProbe.ref,
        addressRef = addressProbe.ref
      )
    )

    f(TestInstances(clientInbox, matcherProbe, addressProbe, wsHandlerRef))
  }

  override protected def afterAll(): Unit = {
    testKit.shutdownTestKit()
    super.afterAll()
  }
}
