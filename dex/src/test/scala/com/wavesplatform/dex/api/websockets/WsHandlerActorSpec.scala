package com.wavesplatform.dex.api.websockets

import java.nio.charset.StandardCharsets
import java.util.Base64

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestInbox}
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter._
import akka.testkit.TestProbe
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
import org.scalatest.concurrent.Eventually
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import scala.concurrent.duration._

class WsHandlerActorSpec extends AnyFreeSpecLike with Matchers with Eventually with MatcherSpecBase with HasJwt {
  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 5.seconds, interval = 100.millis)

  private val testKit = ActorTestKit()
  implicit val ec     = testKit.system.executionContext

  private val issuedAsset = IssuedAsset(ByteStr("issuedAsset".getBytes(StandardCharsets.UTF_8)))
  private val assetPair   = AssetPair(issuedAsset, Waves)

  private val clientKeyPair = KeyPair(ByteStr("seed".getBytes(StandardCharsets.UTF_8)))

  "WsHandlerActor" - {
    "WsOrderBookSubscribe" - {
      "sunny day" in test { t =>
        t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(assetPair, 1))
        t.matcherProbe.expectMsg(
          AggregatedOrderBookEnvelope(
            assetPair,
            AggregatedOrderBookActor.Command.AddWsSubscription(t.clientInbox.ref)
          ))
      }

      "AssetPairBuilder validation" in {
        val invalidAssetPair = AssetPair(Waves, issuedAsset)
        failTest(
          WsOrderBookSubscribe(invalidAssetPair, 1),
          WsError(
            timestamp = 0L,
            code = 9440771, // OrderAssetPairReversed
            message = "The WAVES-T9euE6z4DWHFWxT asset pair should be reversed"
          )
        )
      }

      "negative depth" in failTest(
        WsOrderBookSubscribe(assetPair, -1),
        WsError(
          timestamp = 0L,
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
            AddressActor.WsCommand.AddWsSubscription(t.clientInbox.ref)
          ))
      }

      "jwt is invalid" - {
        "wrong auth type" in failTest(
          WsAddressSubscribe(clientKeyPair, "password", ""),
          WsError(
            timestamp = 0L,
            code = 106960131, // SubscriptionAuthTypeUnsupported
            message = "The subscription authentication type 'password' is not supported. Required one of: jwt"
          )
        )

        "broken" in failTest(
          "dGVzdA==",
          WsError(
            timestamp = 0L,
            code = 110100481, // JwtBroken
            message = "JWT has invalid format"
          )
        )

        "expired" in failTest(
          mkJwt(mkJwtSignedPayload(clientKeyPair, lifetime = -1.minute)),
          WsError(
            timestamp = 0L,
            code = 110105088, // SubscriptionTokenExpired
            message = "The subscription token expired"
          )
        )

        "parsing or validation issues" in failTest(
          "dGVzdA==.dGVzdA==",
          WsError(
            timestamp = 0L,
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
              timestamp = 0L,
              code = 110127617, // JwtPayloadBroken
              message = "JWT payload has not expected fields"
            )
          )
        }

        "wrong network" in failTest(
          mkJwt(mkJwtSignedPayload(clientKeyPair, networkByte = 0)),
          WsError(
            timestamp = 0L,
            code = 110106116, // TokenNetworkUnexpected
            message = "The required network is 84, but given 0"
          )
        )

        "invalid signature" in failTest(
          mkJwt(mkJwtNotSignedPayload(clientKeyPair)),
          WsError(
            timestamp = 0L,
            code = 110103809, // InvalidJwtSignature
            message = "The token signature is invalid"
          )
        )

        "address doesn't fit public key" in failTest(
          WsAddressSubscribe(KeyPair(ByteStr("other-client".getBytes(StandardCharsets.UTF_8))), "jwt", mkJwt(mkJwtSignedPayload(clientKeyPair))),
          WsError(
            timestamp = 0L,
            code = 106957828, // AddressAndPublicKeyAreIncompatible
            message = "Address 3N7nTwcKubzsH6X1uWerLoYFGX1pTKSbhUu and public key D6HmGZqpXCyAqpz8mCAfWijYDWsPKncKe5v3jq1nTpf5 are incompatible"
          )
        )
      }
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
    eventually {
      t.clientInbox.hasMessages shouldBe true
      t.clientInbox.receiveMessage()
    } match {
      case actualError: WsError => actualError should matchTo(expectedError)
      case x                    => fail(s"Unexpected message: $x")
    }
  }

  private case class TestInstances(clientInbox: TestInbox[WsMessage],
                                   matcherProbe: TestProbe,
                                   addressProbe: TestProbe,
                                   wsHandlerRef: ActorRef[WsHandlerActor.Message])

  private def test(f: TestInstances => Unit): Unit = {
    val clientInbox  = TestInbox[WsMessage]()
    val matcherProbe = TestProbe()(testKit.system.toClassic)
    val addressProbe = TestProbe()(testKit.system.toClassic)

    val wsHandlerRef = testKit.spawn(
      WsHandlerActor(
        settings = WsHandlerActor.Settings(10.minutes, 1.minute, 3.minutes, Base64.getEncoder.encodeToString(authServiceKeyPair.getPublic.getEncoded)),
        time = zeroTime,
        assetPairBuilder = new AssetPairBuilder(
          matcherSettings,
          assetDescription = x => effect.liftValueAsync(BriefAssetDescription(x.toString, 8, hasScript = false)),
          blacklistedAssets = Set.empty
        ),
        clientRef = clientInbox.ref,
        matcherRef = matcherProbe.ref,
        addressRef = addressProbe.ref
      ))

    f(TestInstances(clientInbox, matcherProbe, addressProbe, wsHandlerRef))
  }

  override protected def afterAll(): Unit = {
    testKit.shutdownTestKit()
    super.afterAll()
  }
}
