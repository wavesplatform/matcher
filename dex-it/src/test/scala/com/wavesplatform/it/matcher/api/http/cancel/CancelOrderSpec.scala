package com.wavesplatform.it.matcher.api.http.cancel

import com.google.common.primitives.Longs
import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class CancelOrderSpec extends MatcherSuiteBase with RawHttpChecks {

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

  "POST /matcher/orderbook/{amountAsset}/{priceAsset}/cancel" - {
    "should cancel order" in {

      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      placeAndAwaitAtDex(o)

      val r = validate200Json(dex1.rawApi.cancelOrder(alice, o))

      r.orderId should be(o.id())
      r.success should be(true)
      r.status should be("OrderCanceled")

      dex1.api.waitForOrderStatus(o, Status.Cancelled)
    }

    "should return error when order already canceled" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      placeAndAwaitAtDex(o)

      cancelAndAwait(alice, o)
      validateMatcherError(dex1.rawApi.cancelOrder(alice, o), StatusCodes.BadRequest, 9437194, s"The order ${o.idStr()} is canceled")
    }

    //TODO: DEX-1024
    "should return error when order doesn't exist" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      validateMatcherError(dex1.rawApi.cancelOrder(alice, o), StatusCodes.BadRequest, 9437193, s"The order ${o.idStr()} not found")
    }

    "should return an error if publicKey parameter has the different value of used in signature" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      validateIncorrectSignature(dex1.rawApi.cancelOrder(bob, "WAVES", UsdId.toString, o.idStr(), ts, sign))
    }

    "should return an error if timestamp header has the different value of used in signature" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      validateIncorrectSignature(dex1.rawApi.cancelOrder(bob, "WAVES", UsdId.toString, o.idStr(), ts + 1000, sign))
    }
  }

}
