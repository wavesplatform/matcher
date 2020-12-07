package com.wavesplatform.it.matcher.api.http.place

import com.softwaremill.sttp.StatusCodes
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulPlace
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import play.api.libs.json.Json

class PlaceLimitOrderSpec extends PlaceOrderBaseSpec {

  "POST /matcher/orderbook" - {

    "should place order" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

      validate200Json(dex1.rawApi.place(o)) should matchTo(HttpSuccessfulPlace(o))
    }

    "should return error with incorrect order signature" in {
      validateMatcherErrorContainText(
        dex1.rawApi.place(mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd).json().deepMerge(Json.obj("amount" -> 3.waves))),
        StatusCodes.BadRequest,
        9440512,
        s"The signature of order"
      )
    }

    forAll(orderCases) { (n: Int, order: Order, code: Int, error: MatcherError) =>
      s"Case $n: For order [$order] should return error [$error]" in {
        validateMatcherError(dex1.rawApi.place(order), code, error)
      }
    }

  }
}
