package com.wavesplatform.dex.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.testkit.{TestActor, TestProbe}
import com.google.common.primitives.Longs
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.dex._
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.http.RouteSpec
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.{RequestGen, WithDB, crypto}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsString, JsValue}

import scala.concurrent.Future

class MatcherApiRouteSpec extends RouteSpec("/matcher") with RequestGen with PathMockFactory with Eventually with WithDB {

  private val settings                       = MatcherSettings.valueReader.read(ConfigFactory.load(), "waves.dex")
  private val matcherKeyPair                 = KeyPair("matcher".getBytes("utf-8"))
  private def getAssetDecimals(asset: Asset) = 8
  private val smartAssetTx = smartIssueTransactionGen().retryUntil(_.script.nonEmpty).sample.get
  private val smartAssetDesc = AssetDescription(
    issuer = smartAssetTx.sender,
    name = smartAssetTx.name,
    description = smartAssetTx.description,
    decimals = smartAssetTx.decimals,
    reissuable = smartAssetTx.reissuable,
    totalVolume = smartAssetTx.quantity,
    script = smartAssetTx.script,
    sponsorship = 0
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
    val apiKey = "apiKey"
    val rate = 0.0055
    val updatedRate = 0.0067

    val rateCache = RateCache.inMem
    "add rate" in test({ route =>
      Put(routePath(s"/settings/rates/${smartAssetTx.id()}"), rate).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldEqual StatusCodes.Created
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual s"Rate $rate for the asset ${smartAssetTx.id()} added"
        rateCache.getAllRates(IssuedAsset(smartAssetTx.id())) shouldBe rate
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))), rateCache)

    "update rate" in test({ route =>
      Put(routePath(s"/settings/rates/${smartAssetTx.id()}"), updatedRate).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual s"Rate for the asset ${smartAssetTx.id()} updated, old value = $rate, new value = $updatedRate"
        rateCache.getAllRates(IssuedAsset(smartAssetTx.id())) shouldBe updatedRate
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))), rateCache)

    "delete rate" in test({ route =>
      Delete(routePath(s"/settings/rates/${smartAssetTx.id()}")).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldEqual StatusCodes.OK
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual s"Rate for the asset ${smartAssetTx.id()} deleted, old value = $updatedRate"
        rateCache.getAllRates.keySet should not contain IssuedAsset(smartAssetTx.id())
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))), rateCache)

    "changing waves rate" in test({ route =>
      Put(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldBe StatusCodes.BadRequest
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual "Rate for Waves cannot be changed"
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))))

    "change rates without api key" in test({ route =>
      Put(routePath("/settings/rates/WAVES"), rate) ~> route ~> check {
        status shouldBe StatusCodes.Forbidden
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual "Provided API key is not correct"
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))))

    "change rates with wrong api key" in test({ route =>
      Put(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldBe StatusCodes.Forbidden
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual "Provided API key is not correct"
      }
    }, Option(crypto.secureHash("wrongApiKey".getBytes("UTF-8"))))

    "deleting waves rate" in test({ route =>
      Delete(routePath("/settings/rates/WAVES")).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldBe StatusCodes.BadRequest
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual "Rate for Waves cannot be deleted"
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))))

    "delete rates without api key" in test({ route =>
      Delete(routePath("/settings/rates/WAVES"), rate) ~> route ~> check {
        status shouldBe StatusCodes.Forbidden
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual "Provided API key is not correct"
      }
    }, Option(crypto.secureHash(apiKey.getBytes("UTF-8"))))

    "delete rates with wrong api key" in test({ route =>
      Delete(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY",  apiKey)) ~> route ~> check {
        status shouldBe StatusCodes.Forbidden
        val message = (responseAs[JsValue] \ "message").as[JsString]
        message.value shouldEqual "Provided API key is not correct"
      }
    }, Option(crypto.secureHash("wrongApiKey".getBytes("UTF-8"))))
  }

  private def test[T](f: Route => T, apiKeyHash: Option[Array[Byte]] = None, rateCache: RateCache = RateCache.inMem): T = {
    val blockchain   = stub[Blockchain]
    val addressActor = TestProbe("address")
    (blockchain.assetDescription _).when(IssuedAsset(smartAssetTx.id())).onCall((_: IssuedAsset) => Some(smartAssetDesc))
    addressActor.setAutoPilot { (sender: ActorRef, msg: Any) =>
      msg match {
        case AddressDirectory.Envelope(_, AddressActor.GetReservedBalance) => sender ! Map.empty[Asset, Long]
        case _                                                             =>
      }

      TestActor.NoAutoPilot
    }

    implicit val context = new com.wavesplatform.dex.error.ErrorFormatterContext {
      override def assetDecimals(asset: Asset): Int = 8
    }

    val route = MatcherApiRoute(
      assetPairBuilder = new AssetPairBuilder(settings, blockchain, Set.empty),
      matcherPublicKey = matcherKeyPair.publicKey,
      matcher = ActorRef.noSender,
      addressActor = addressActor.ref,
      storeEvent = _ => Future.failed(new NotImplementedError("Storing is not implemented")),
      orderBook = _ => None,
      getMarketStatus = _ => None,
      tickSize = _ => 0.1,
      orderValidator = _ => Left(error.FeatureNotImplemented),
      orderBookSnapshot = new OrderBookSnapshotHttpCache(settings.orderBookSnapshotHttpCache, ntpTime, getAssetDecimals, _ => None),
      matcherSettings = settings,
      matcherStatus = () => Matcher.Status.Working,
      db = db,
      time = ntpTime,
      currentOffset = () => 0L,
      lastOffset = () => Future.successful(0L),
      matcherAccountFee = 300000L,
      apiKeyHash = apiKeyHash,
      rateCache = rateCache,
      validatedAllowedOrderVersions = Set(1, 2, 3)
    ).route

    f(route)
  }

}
