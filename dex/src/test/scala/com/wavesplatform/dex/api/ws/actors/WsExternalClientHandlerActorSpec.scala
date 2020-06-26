package com.wavesplatform.dex.api.ws.actors

import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe => TypedTestProbe}
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter._
import akka.testkit.TestProbe
import cats.syntax.either._
import com.wavesplatform.dex._
import com.wavesplatform.dex.actors.MatcherActor.AggregatedOrderBookEnvelope
import com.wavesplatform.dex.actors.address.AddressActor.WsCommand
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor.Command
import com.wavesplatform.dex.api.ws.HasJwt
import com.wavesplatform.dex.api.ws.actors.WsExternalClientHandlerActor.Command.ProcessClientMessage
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.SubscriptionsLimitReached
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.AssetPairBuilder
import com.wavesplatform.dex.settings.SubscriptionsSettings
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import scala.concurrent.duration._

class WsExternalClientHandlerActorSpec extends AnyFreeSpecLike with Matchers with MatcherSpecBase with HasJwt {

  private val testKit = ActorTestKit()

  private implicit val ec = testKit.system.executionContext

  private val issuedAsset   = IssuedAsset(ByteStr("issuedAsset".getBytes(StandardCharsets.UTF_8)))
  private val assetPair     = AssetPair(issuedAsset, Waves)
  private val clientKeyPair = mkKeyPair("seed")

  private val subscriptionsSettings = SubscriptionsSettings.default

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

      "should not have effect if client requests a lot of subscriptions for the same asset pair" in test { t =>
        def subscribeAssetPair(): Unit = t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(assetPair, 1))

        subscribeAssetPair()
        t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(assetPair, Command.AddWsSubscription(t.clientProbe.ref)))

        (1 to 10) foreach (_ => subscribeAssetPair())
        t.matcherProbe.expectNoMessage()
      }
    }

    "WsAddressSubscribe" - {
      "sunny day" in test { t =>
        val jwtPayload = mkJwtSignedPayload(clientKeyPair)
        t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, mkJwt(jwtPayload)))
        t.addressProbe.expectMsg(
          AddressDirectoryActor.Envelope(
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
      val jwtPayload = mkJwtSignedPayload(clientKeyPair)
      val clientRef  = t.clientProbe.ref

      t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(assetPair, 1))
      t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(clientKeyPair, WsAddressSubscribe.defaultAuthType, mkJwt(jwtPayload)))

      t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(assetPair, Command.AddWsSubscription(clientRef)))
      t.addressProbe.expectMsg(AddressDirectoryActor.Envelope(clientKeyPair, WsCommand.AddWsSubscription(clientRef)))

      t.wsHandlerRef ! WsExternalClientHandlerActor.Event.Completed(().asRight)

      t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(assetPair, Command.RemoveWsSubscription(clientRef)))
      t.addressProbe.expectMsg(AddressDirectoryActor.Envelope(clientKeyPair, WsCommand.RemoveWsSubscription(clientRef)))
    }

    "should close old order book subscriptions if total order book subscriptions number has reached limit" in test { t =>
      val assetPairs = (1 to subscriptionsSettings.maxOrderBookNumber).map(idx => AssetPair(IssuedAsset(s"ia-$idx".getBytes), Waves))

      assetPairs.foreach { assetPair =>
        t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(assetPair, 1))
        t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(assetPair, Command.AddWsSubscription(t.clientProbe.ref)))
      }

      def checkEviction(newSubscription: AssetPair, oldSubscription: AssetPair): Unit = {
        t.wsHandlerRef ! ProcessClientMessage(WsOrderBookSubscribe(newSubscription, 1))
        t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(newSubscription, Command.AddWsSubscription(t.clientProbe.ref)))

        t.matcherProbe.expectMsg(AggregatedOrderBookEnvelope(oldSubscription, Command.RemoveWsSubscription(t.clientProbe.ref)))
        t.clientProbe.expectMessageType[WsError] should matchTo {
          WsError.from(SubscriptionsLimitReached(subscriptionsSettings.maxOrderBookNumber, oldSubscription.toString), time.getTimestamp())
        }
      }

      assetPairs.foldLeft(assetPair) { case (newSubscription, oldSubscription) => checkEviction(newSubscription, oldSubscription); oldSubscription }
    }

    "should close old address subscriptions if total address subscriptions number has reached limit" in test { t =>
      val keyPairs = (1 to subscriptionsSettings.maxAddressNumber).map(idx => mkKeyPair(idx.toString))

      def sendSubscriptionRequest(keyPair: KeyPair): Unit = {
        t.wsHandlerRef ! ProcessClientMessage(WsAddressSubscribe(keyPair, WsAddressSubscribe.defaultAuthType, mkJwt(mkJwtSignedPayload(keyPair))))
        t.addressProbe.expectMsg(AddressDirectoryActor.Envelope(keyPair, WsCommand.AddWsSubscription(t.clientProbe.ref)))
      }

      keyPairs.foreach(sendSubscriptionRequest)

      def checkEviction(newSubscription: KeyPair, oldSubscription: KeyPair): Unit = {
        sendSubscriptionRequest(newSubscription)
        t.addressProbe.expectMsg(AddressDirectoryActor.Envelope(oldSubscription, WsCommand.RemoveWsSubscription(t.clientProbe.ref)))
        t.clientProbe.expectMessageType[WsError] should matchTo {
          WsError.from(SubscriptionsLimitReached(subscriptionsSettings.maxAddressNumber, oldSubscription.toAddress.toString), time.getTimestamp())
        }
      }

      keyPairs.foldLeft(clientKeyPair) { case (newSubscription, oldSubscription) => checkEviction(newSubscription, oldSubscription); oldSubscription }
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
                                   wsHandlerRef: ActorRef[WsExternalClientHandlerActor.Message],
                                   connectionId: String)

  private def test(f: TestInstances => Unit): Unit = {

    val clientInbox  = TypedTestProbe[WsMessage]()(testKit.system)
    val matcherProbe = TestProbe(UUID.randomUUID().toString)(testKit.system.toClassic)
    val addressProbe = TestProbe(UUID.randomUUID().toString)(testKit.system.toClassic)

    val connectionId = UUID.randomUUID().toString

    val wsHandlerRef = testKit.spawn(
      WsExternalClientHandlerActor(
        settings = WsExternalClientHandlerActor.Settings(
          messagesInterval = 100.millis,
          maxConnectionLifetime = 10.minutes,
          jwtPublicKey = Base64.getEncoder.encodeToString(authServiceKeyPair.getPublic.getEncoded),
          subscriptions = subscriptionsSettings,
          healthCheck = WsHealthCheckSettings(
            pingInterval = 1.minute,
            pongTimeout = 3.minutes
          )
        ),
        time = time,
        assetPairBuilder = new AssetPairBuilder(
          matcherSettings,
          assetDescription = x => effect.liftValueAsync(BriefAssetDescription(x.toString, 8, hasScript = false)),
          blacklistedAssets = Set.empty
        ),
        clientRef = clientInbox.ref,
        matcherRef = matcherProbe.ref,
        addressRef = addressProbe.ref,
        connectionId = connectionId
      )
    )

    clientInbox.expectMessageType[WsInitial].connectionId should matchTo(connectionId)
    f(TestInstances(clientInbox, matcherProbe, addressProbe, wsHandlerRef, connectionId))
  }

  override protected def afterAll(): Unit = {
    testKit.shutdownTestKit()
    super.afterAll()
  }
}
