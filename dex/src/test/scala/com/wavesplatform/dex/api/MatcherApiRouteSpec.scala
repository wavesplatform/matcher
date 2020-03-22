package com.wavesplatform.dex.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.testkit.{TestActor, TestProbe}
import com.google.common.primitives.Longs
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex._
import com.wavesplatform.dex.api.http.ApiMarshallers._
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.db.{OrderDB, WithDB}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.effect._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsString, JsValue, Json}

import scala.concurrent.Future
import scala.util.Random

class MatcherApiRouteSpec extends RouteSpec("/matcher") with MatcherSpecBase with PathMockFactory with Eventually with WithDB {

  private val settings       = MatcherSettings.valueReader.read(ConfigFactory.load(), "waves.dex")
  private val apiKey         = "apiKey"
  private val matcherKeyPair = KeyPair("matcher".getBytes("utf-8"))
  private val smartAsset     = arbitraryAssetGen.sample.get
  private val smartAssetId   = smartAsset.id

  // Will be refactored in DEX-548
  private val orderToCancel = orderV3Generator.sample.get

  private val smartAssetDesc = BriefAssetDescription(
    name = "smart asset",
    decimals = Random.nextInt(9),
    hasScript = false
  )

  routePath("/balance/reserved/{publicKey}") - {

    val publicKey = matcherKeyPair.publicKey
    val ts        = System.currentTimeMillis()
    val signature = crypto.sign(matcherKeyPair, publicKey ++ Longs.toByteArray(ts))

    def mkGet(route: Route)(base58PublicKey: String, ts: Long, base58Signature: String): RouteTestResult =
      Get(routePath(s"/balance/reserved/$base58PublicKey")).withHeaders(
        RawHeader("Timestamp", s"$ts"),
        RawHeader("Signature", base58Signature)
      ) ~> route

    "returns a reserved balance for specified publicKey" in test { route =>
      mkGet(route)(Base58.encode(publicKey), ts, Base58.encode(signature)) ~> check {
        status shouldBe StatusCodes.OK
      }
    }

    "returns HTTP 400 when provided a wrong base58-encoded" - {
      "signature" in test { route =>
        mkGet(route)(Base58.encode(publicKey), ts, ";;") ~> check {
          status shouldBe StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "The request has an invalid signature"
        }
      }

      "public key" in test { route =>
        mkGet(route)(";;", ts, Base58.encode(signature)) ~> check {
          handled shouldBe false
        }
      }
    }
  }

  routePath("/settings/rates/{assetId}") - {

    val rateCache = RateCache.inMem

    val rate        = 0.0055
    val updatedRate = 0.0067

    "add rate" in test(
      { route =>
        Put(routePath(s"/settings/rates/$smartAssetId"), rate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.Created
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual s"The rate $rate for the asset $smartAssetId added"
          rateCache.getAllRates(smartAsset) shouldBe rate
        }
      },
      apiKey,
      rateCache
    )

    "update rate" in test(
      { route =>
        Put(routePath(s"/settings/rates/$smartAssetId"), updatedRate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual s"The rate for the asset $smartAssetId updated, old value = $rate, new value = $updatedRate"
          rateCache.getAllRates(smartAsset) shouldBe updatedRate
        }
      },
      apiKey,
      rateCache
    )

    "update rate incorrectly (incorrect body)" in test(
      { route =>
        Put(routePath(s"/settings/rates/$smartAssetId"), "qwe").withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "The provided JSON is invalid. Check the documentation"
        }
      },
      apiKey,
      rateCache
    )

    "update rate incorrectly (incorrect value)" in test(
      { route =>
        Put(routePath(s"/settings/rates/$smartAssetId"), 0).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "Asset rate should be positive"
        }
      },
      apiKey,
      rateCache
    )

    "update rate incorrectly (incorrect content type)" in test(
      { route =>
        Put(routePath(s"/settings/rates/$smartAssetId"), HttpEntity(ContentTypes.`text/plain(UTF-8)`, "5"))
          .withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "The provided JSON is invalid. Check the documentation"
        }
      },
      apiKey,
      rateCache
    )

    "delete rate" in test(
      { route =>
        Delete(routePath(s"/settings/rates/$smartAssetId")).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual s"The rate for the asset $smartAssetId deleted, old value = $updatedRate"
          rateCache.getAllRates.keySet should not contain smartAsset
        }
      },
      apiKey,
      rateCache
    )

    "changing waves rate" in test(
      { route =>
        Put(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldBe StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "The rate for WAVES cannot be changed"
        }
      },
      apiKey
    )

    "change rates without api key" in test(
      { route =>
        Put(routePath("/settings/rates/WAVES"), rate) ~> route ~> check {
          status shouldBe StatusCodes.Forbidden
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "Provided API key is not correct"
        }
      },
      apiKey
    )

    "change rates with wrong api key" in test(
      { route =>
        Put(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY", "wrongApiKey")) ~> route ~> check {
          status shouldBe StatusCodes.Forbidden
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "Provided API key is not correct"
        }
      },
      apiKey
    )

    "deleting waves rate" in test(
      { route =>
        Delete(routePath("/settings/rates/WAVES")).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldBe StatusCodes.BadRequest
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "The rate for WAVES cannot be changed"
        }
      },
      apiKey
    )

    "delete rates without api key" in test(
      { route =>
        Delete(routePath("/settings/rates/WAVES")) ~> route ~> check {
          status shouldBe StatusCodes.Forbidden
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "Provided API key is not correct"
        }
      },
      apiKey
    )

    "delete rates with wrong api key" in test(
      { route =>
        Delete(routePath("/settings/rates/WAVES")).withHeaders(RawHeader("X-API-KEY", "wrongApiKey")) ~> route ~> check {
          status shouldBe StatusCodes.Forbidden
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual "Provided API key is not correct"
        }
      },
      apiKey
    )

    "delete rate for the asset that doesn't have rate" in test(
      { route =>
        rateCache.deleteRate(smartAsset)
        Delete(routePath(s"/settings/rates/$smartAssetId")).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldBe StatusCodes.NotFound
          val message = (responseAs[JsValue] \ "message").as[JsString]
          message.value shouldEqual s"The rate for the asset $smartAssetId was not specified"
        }
      },
      apiKey,
      rateCache
    )
  }

  routePath("/orderbook") - {
    "invalid field" in test { route =>
      // amount is too long
      val orderJson =
        """{
          |  "version": 1,
          |  "id": "6XHKohY1Wh8HwFx9SAf8CYwiRYBxPpWAZkHen6Whwu3i",
          |  "sender": "3N2EPHQ8hU3sFUBGcWfaS91yLpRgdQ6R8CE",
          |  "senderPublicKey": "Frfv91pfd4HUa9PxDQhyLo2nuKKtn49yMVXKpKN4gjK4",
          |  "matcherPublicKey": "77J1rZi6iyizrjH6SR9iyiKWU99MTvujDS5LUuPPqeEr",
          |  "assetPair": {
          |    "amountAsset": "7XxvP6RtKcMYEVrKZwJcaLwek4FjGkL3hWKRA6r44Pp",
          |    "priceAsset": "BbDpaEUT1R1S5fxScefViEhPmrT7rPvWhU9eYB4masS"
          |  },
          |  "orderType": "buy",
          |  "amount": 2588809419424100000000000000,
          |  "price": 22375150522026,
          |  "timestamp": 1002536707239093185,
          |  "expiration": 1576213723344,
          |  "matcherFee": 2412058533372,
          |  "signature": "4a4JP1pKtrZ5Vts2qZ9guJXsyQJaFxhJHoskzxP7hSUtDyXesFpY66REmxeDe5hUeXXMSkPP46vJXxxDPhv7hzfm",
          |  "proofs": [
          |    "4a4JP1pKtrZ5Vts2qZ9guJXsyQJaFxhJHoskzxP7hSUtDyXesFpY66REmxeDe5hUeXXMSkPP46vJXxxDPhv7hzfm"
          |  ]
          |}""".stripMargin

      Post(routePath("/orderbook"), HttpEntity(ContentTypes.`application/json`, orderJson)) ~> route ~> check {
        status shouldEqual StatusCodes.BadRequest
        val json = responseAs[JsValue]
        (json \ "error").as[Int] shouldBe 1048577
        (json \ "params" \ "invalidFields").as[List[String]] shouldBe List("/amount")
      }
    }

    "completely invalid JSON" in test { route =>
      val orderJson = "{ I AM THE DANGEROUS HACKER"

      Post(routePath("/orderbook"), HttpEntity(ContentTypes.`application/json`, orderJson)) ~> route ~> check {
        status shouldEqual StatusCodes.BadRequest
        val json = responseAs[JsValue]
        (json \ "error").as[Int] shouldBe 1048577
        (json \ "message").as[String] shouldBe "The provided JSON is invalid. Check the documentation"
      }
    }
  }

  // cancelAllById
  routePath("/orders/cancel") - {
    val orderId = orderToCancel.id()

    "X-Api-Key is required" in test { route =>
      Post(routePath("/orders/cancel"), HttpEntity(ContentTypes.`application/json`, Json.toJson(Set(orderId)).toString())) ~> route ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
    }

    "X-User-Public-Key is required" in test(
      { route =>
        Post(
          routePath("/orders/cancel"),
          HttpEntity(ContentTypes.`application/json`, Json.toJson(Set(orderId)).toString())
        ).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
        }
      },
      apiKey
    )

    "sunny day" in test(
      { route =>
        Post(
          routePath("/orders/cancel"),
          HttpEntity(ContentTypes.`application/json`, Json.toJson(Set(orderId)).toString())
        ).withHeaders(RawHeader("X-API-KEY", apiKey), RawHeader("X-User-Public-Key", orderToCancel.senderPublicKey.base58)) ~> route ~> check {
          status shouldEqual StatusCodes.OK
        }
      },
      apiKey
    )
  }

  // forceCancelOrder
  routePath("/orders/cancel/[orderId]") - {
    val orderId = orderToCancel.id()

    "X-Api-Key is required" in test { route =>
      Post(routePath(s"/orders/cancel/$orderId")) ~> route ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
    }

    "X-User-Public-Key is not required" in test(
      { route =>
        Post(routePath(s"/orders/cancel/$orderId")).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
          status shouldEqual StatusCodes.OK
        }
      },
      apiKey
    )

    "X-User-Public-Key is specified, but wrong" in test(
      { route =>
        Post(routePath(s"/orders/cancel/$orderId"))
          .withHeaders(RawHeader("X-API-KEY", apiKey), RawHeader("X-User-Public-Key", matcherKeyPair.publicKey.base58)) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
        }
      },
      apiKey
    )

    "sunny day" in test(
      { route =>
        Post(routePath(s"/orders/cancel/$orderId"))
          .withHeaders(RawHeader("X-API-KEY", apiKey), RawHeader("X-User-Public-Key", orderToCancel.senderPublicKey.base58)) ~> route ~> check {
          status shouldEqual StatusCodes.OK
        }
      },
      apiKey
    )
  }

  private def test[U](f: Route => U, apiKey: String = "", rateCache: RateCache = RateCache.inMem): U = {

    val addressActor = TestProbe("address")

    addressActor.setAutoPilot { (sender: ActorRef, msg: Any) =>
      msg match {
        case AddressDirectory.Envelope(_, AddressActor.Query.GetReservedBalance) => sender ! AddressActor.Reply.Balance(Map.empty)
        case AddressDirectory.Envelope(address, command: AddressActor.Command.CancelOrders) if address == orderToCancel.sender.toAddress =>
          sender ! api.BatchCancelCompleted { command.orderIds.map(id => id -> api.OrderCanceled(id)).toMap }
        case AddressDirectory.Envelope(address, command: AddressActor.Command.CancelOrder) if address == orderToCancel.sender.toAddress =>
          sender ! api.OrderCanceled(command.orderId)
        case _ =>
      }

      TestActor.NoAutoPilot
    }

    val odb = OrderDB(settings.orderDb, db)
    odb.saveOrder(orderToCancel)

    val route: Route = MatcherApiRoute(
      assetPairBuilder = new AssetPairBuilder(
        settings,
        x => {
          if (x == smartAsset)
            liftValueAsync[BriefAssetDescription](BriefAssetDescription(smartAssetDesc.name, smartAssetDesc.decimals, hasScript = false))
          else liftErrorAsync[BriefAssetDescription](error.AssetNotFound(x))
        },
        Set.empty
      ),
      matcherPublicKey = matcherKeyPair.publicKey,
      matcher = ActorRef.noSender,
      addressActor = addressActor.ref,
      storeEvent = _ => Future.failed(new NotImplementedError("Storing is not implemented")),
      orderBook = _ => None,
      getMarketStatus = _ => None,
      getActualTickSize = _ => 0.1,
      orderValidator = _ => liftErrorAsync { error.FeatureNotImplemented },
      orderBookSnapshot = new OrderBookSnapshotHttpCache(
        settings.orderBookSnapshotHttpCache,
        ntpTime,
        x => if (x == smartAsset) Some(smartAssetDesc.decimals) else throw new IllegalArgumentException(s"No information about $x"),
        _ => None
      ),
      matcherSettings = settings,
      matcherStatus = () => Matcher.Status.Working,
      db = db,
      time = ntpTime,
      currentOffset = () => 0L,
      lastOffset = () => Future.successful(0L),
      matcherAccountFee = 300000L,
      apiKeyHash = Some(crypto secureHash apiKey),
      rateCache = rateCache,
      validatedAllowedOrderVersions = () => Future.successful { Set(1, 2, 3) },
      () => DynamicSettings.symmetric(matcherFee)
    ).route

    f(route)
  }
}
