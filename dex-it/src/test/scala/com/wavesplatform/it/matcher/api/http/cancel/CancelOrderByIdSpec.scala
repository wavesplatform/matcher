package com.wavesplatform.it.matcher.api.http.cancel

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.error.{InvalidBase58String, OrderCanceled, OrderNotFound, UserPublicKeyIsNotValid}
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class CancelOrderByIdSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "POST /matcher/orders/cancel/{id}" - {
    "should cancel order by id" in {

      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      placeAndAwaitAtDex(order)

      val r = validate200Json(dex1.rawApi.cancelOrderById(order))

      r.success should be(true)
      r.status should be("OrderCanceled")
      r.orderId should be(order.id())

      dex1.api.waitForOrderStatus(order, Status.Cancelled)
    }

    "should return error when order already cancelled" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      placeAndAwaitAtDex(order)

      dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, order)

      validateMatcherError(
        dex1.rawApi.cancelOrderById(order),
        StatusCode.BadRequest,
        OrderCanceled.code,
        s"The order ${order.idStr()} is canceled"
      )
    }

    //TODO: DEX-1024
    "should return error doesn't exist" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)

      validateMatcherError(dex1.rawApi.cancelOrderById(order), StatusCode.NotFound, OrderNotFound.code, s"The order ${order.idStr()} not found")
    }

    "should return an error when orderId is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOneOrderWithKey("null", Map("X-API-KEY" -> apiKey)),
        StatusCode.BadRequest,
        InvalidBase58String.code,
        "Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error when the public-key header is not a correct base58 string" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      placeAndAwaitAtDex(order)

      validateMatcherError(
        dex1.rawApi.cancelOneOrderWithKey(order.idStr(), Map("X-API-Key" -> apiKey, "X-User-Public-Key" -> "null")),
        StatusCode.Forbidden,
        UserPublicKeyIsNotValid.code,
        "Provided public key is not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    //TODO: DEX-1024
    "should return an error when the public-key header is not from order owner" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      placeAndAwaitAtDex(order)

      validateMatcherError(
        dex1.rawApi.cancelOneOrderWithKey(order.idStr(), Map("X-API-Key" -> apiKey, "X-User-Public-Key" -> bob.publicKey.stringRepr)),
        StatusCode.NotFound,
        OrderNotFound.code,
        s"The order ${order.idStr()} not found"
      )
    }
  }
}
