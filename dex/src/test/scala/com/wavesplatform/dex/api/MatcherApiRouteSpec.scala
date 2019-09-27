//package com.wavesplatform.dex.api
//
//import akka.actor.ActorRef
//import akka.http.scaladsl.model.StatusCodes
//import akka.http.scaladsl.model.headers.RawHeader
//import akka.http.scaladsl.server.Route
//import akka.testkit.{TestActor, TestProbe}
//import com.google.common.primitives.Longs
//import com.typesafe.config.ConfigFactory
//import com.wavesplatform.account.KeyPair
//import com.wavesplatform.common.state.ByteStr
//import com.wavesplatform.common.utils.Base58
//import com.wavesplatform.dex._
//import com.wavesplatform.dex.cache.RateCache
//import com.wavesplatform.dex.error.ErrorFormatterContext
//import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
//import com.wavesplatform.dex.settings.MatcherSettings
//import com.wavesplatform.http.ApiMarshallers._
//import com.wavesplatform.http.RouteSpec
//import com.wavesplatform.transaction.Asset
//import com.wavesplatform.transaction.Asset.IssuedAsset
//import com.wavesplatform.{RequestGen, WithDB, crypto}
//import org.scalamock.scalatest.PathMockFactory
//import org.scalatest.concurrent.Eventually
//import play.api.libs.json.{JsString, JsValue}
//
//import scala.concurrent.Future
//
//class MatcherApiRouteSpec extends RouteSpec("/matcher") with RequestGen with PathMockFactory with Eventually with WithDB {
//
//  private val settings       = MatcherSettings.valueReader.read(ConfigFactory.load(), "waves.dex")
//  private val matcherKeyPair = KeyPair("matcher".getBytes("utf-8"))
//  private val smartAssetTx   = smartIssueTransactionGen().retryUntil(_.script.nonEmpty).sample.get
//  private val asset          = IssuedAsset(smartAssetTx.id())
//
//  private val smartAssetDesc = BriefAssetDescription(
//    name = ByteStr(smartAssetTx.name),
//    decimals = smartAssetTx.decimals,
//    hasScript = false
//  )
//
//  routePath("/balance/reserved/{publicKey}") - {
//
//    val publicKey = matcherKeyPair.publicKey
//    val ts        = System.currentTimeMillis()
//    val signature = crypto.sign(matcherKeyPair, publicKey ++ Longs.toByteArray(ts))
//
//    def mkGet(route: Route)(base58PublicKey: String, ts: Long, base58Signature: String): RouteTestResult =
//      Get(routePath(s"/balance/reserved/$base58PublicKey")).withHeaders(
//        RawHeader("Timestamp", s"$ts"),
//        RawHeader("Signature", base58Signature)
//      ) ~> route
//
//    "returns a reserved balance for specified publicKey" in test { route =>
//      mkGet(route)(Base58.encode(publicKey), ts, Base58.encode(signature)) ~> check {
//        status shouldBe StatusCodes.OK
//      }
//    }
//
//    "returns HTTP 400 when provided a wrong base58-encoded" - {
//      "signature" in test { route =>
//        mkGet(route)(Base58.encode(publicKey), ts, ";;") ~> check {
//          status shouldBe StatusCodes.BadRequest
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "The request has an invalid signature"
//        }
//      }
//
//      "public key" in test { route =>
//        mkGet(route)(";;", ts, Base58.encode(signature)) ~> check {
//          handled shouldBe false
//        }
//      }
//    }
//  }
//
//  routePath("/settings/rates/{assetId}") - {
//
//    val apiKey    = "apiKey"
//    val rateCache = RateCache.inMem
//
//    val rate        = 0.0055
//    val updatedRate = 0.0067
//
//    "add rate" in test(
//      { route =>
//        Put(routePath(s"/settings/rates/${smartAssetTx.id()}"), rate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldEqual StatusCodes.Created
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual s"The rate $rate for the asset ${smartAssetTx.id()} added"
//          rateCache.getAllRates(IssuedAsset(smartAssetTx.id())) shouldBe rate
//        }
//      },
//      apiKey,
//      rateCache
//    )
//
//    "update rate" in test(
//      { route =>
//        Put(routePath(s"/settings/rates/${smartAssetTx.id()}"), updatedRate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldEqual StatusCodes.OK
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual s"The rate for the asset ${smartAssetTx.id()} updated, old value = $rate, new value = $updatedRate"
//          rateCache.getAllRates(IssuedAsset(smartAssetTx.id())) shouldBe updatedRate
//        }
//      },
//      apiKey,
//      rateCache
//    )
//
//    "delete rate" in test(
//      { route =>
//        Delete(routePath(s"/settings/rates/${smartAssetTx.id()}")).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldEqual StatusCodes.OK
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual s"The rate for the asset ${smartAssetTx.id()} deleted, old value = $updatedRate"
//          rateCache.getAllRates.keySet should not contain IssuedAsset(smartAssetTx.id())
//        }
//      },
//      apiKey,
//      rateCache
//    )
//
//    "changing waves rate" in test(
//      { route =>
//        Put(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldBe StatusCodes.BadRequest
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "The rate for WAVES cannot be changed"
//        }
//      },
//      apiKey
//    )
//
//    "change rates without api key" in test(
//      { route =>
//        Put(routePath("/settings/rates/WAVES"), rate) ~> route ~> check {
//          status shouldBe StatusCodes.Forbidden
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "Provided API key is not correct"
//        }
//      },
//      apiKey
//    )
//
//    "change rates with wrong api key" in test(
//      { route =>
//        Put(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldBe StatusCodes.Forbidden
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "Provided API key is not correct"
//        }
//      },
//      "wrongApiKey"
//    )
//
//    "deleting waves rate" in test(
//      { route =>
//        Delete(routePath("/settings/rates/WAVES")).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldBe StatusCodes.BadRequest
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "The rate for WAVES cannot be changed"
//        }
//      },
//      apiKey
//    )
//
//    "delete rates without api key" in test(
//      { route =>
//        Delete(routePath("/settings/rates/WAVES"), rate) ~> route ~> check {
//          status shouldBe StatusCodes.Forbidden
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "Provided API key is not correct"
//        }
//      },
//      apiKey
//    )
//
//    "delete rates with wrong api key" in test(
//      { route =>
//        Delete(routePath("/settings/rates/WAVES"), rate).withHeaders(RawHeader("X-API-KEY", apiKey)) ~> route ~> check {
//          status shouldBe StatusCodes.Forbidden
//          val message = (responseAs[JsValue] \ "message").as[JsString]
//          message.value shouldEqual "Provided API key is not correct"
//        }
//      },
//      "wrongApiKey"
//    )
//  }
//
//  private def test[U](f: Route => U, apiKey: String = "", rateCache: RateCache = RateCache.inMem): U = {
//    val addressActor = TestProbe("address")
//    addressActor.setAutoPilot { (sender: ActorRef, msg: Any) =>
//      msg match {
//        case AddressDirectory.Envelope(_, AddressActor.GetReservedBalance) => sender ! Map.empty[Asset, Long]
//        case _                                                             =>
//      }
//
//      TestActor.NoAutoPilot
//    }
//
//    implicit val context: ErrorFormatterContext = _ => 8
//
//    val route: Route = MatcherApiRoute(
//      assetPairBuilder = new AssetPairBuilder(
//        settings,
//        x =>
//          if (x == asset) Some(BriefAssetDescription(name = ByteStr(smartAssetDesc.name), decimals = smartAssetDesc.decimals, hasScript = false))
//          else None,
//        Set.empty
//      ),
//      matcherPublicKey = matcherKeyPair.publicKey,
//      matcher = ActorRef.noSender,
//      addressActor = addressActor.ref,
//      storeEvent = _ => Future.failed(new NotImplementedError("Storing is not implemented")),
//      orderBook = _ => None,
//      getMarketStatus = _ => None,
//      tickSize = _ => 0.1,
//      orderValidator = _ => Left(error.FeatureNotImplemented),
//      orderBookSnapshot = new OrderBookSnapshotHttpCache(
//        settings.orderBookSnapshotHttpCache,
//        ntpTime,
//        x => if (x == asset) smartAssetDesc.decimals else throw new IllegalArgumentException(s"No information about $x"),
//        _ => None
//      ),
//      matcherSettings = settings,
//      matcherStatus = () => Matcher.Status.Working,
//      db = db,
//      time = ntpTime,
//      currentOffset = () => 0L,
//      lastOffset = () => Future.successful(0L),
//      matcherAccountFee = () => 300000L,
//      apiKeyHashStr = Base58.encode(crypto.secureHash(apiKey.getBytes("UTF-8"))),
//      rateCache = rateCache,
//      validatedAllowedOrderVersions = Set(1, 2, 3)
//    ).route
//
//    f(route)
//  }
//}
