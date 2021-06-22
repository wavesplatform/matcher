package com.wavesplatform.it.matcher.api.http.place

import sttp.model.StatusCode
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulPlace
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.error.OrderInvalidSignature
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import play.api.libs.json.Json

class PlaceMarketOrderSpec extends PlaceOrderBaseSpec {

  "POST /matcher/orderbook/market" - {

    "should place order" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

      validate200Json(dex1.rawApi.placeMarket(o)) should matchTo(HttpSuccessfulPlace(o))
    }

    "should return error with incorrect order signature" in {
      validateMatcherErrorContainText(
        dex1.rawApi.placeMarket(mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd).json().deepMerge(Json.obj("amount" -> 3.waves))),
        StatusCode.BadRequest,
        OrderInvalidSignature.code,
        s"The signature of order"
      )
    }

    forAll(orderCases) { (n: Int, order: Order, code: StatusCode, error: MatcherError) =>
      s"Case $n: For order [$order] should return error [$error]" in {
        validateMatcherError(dex1.rawApi.placeMarket(order), code, error)
      }
    }

  }
}
