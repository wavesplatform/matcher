package com.wavesplatform.it.matcher.api.http.cancel

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulSingleCancel
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class CancelAllOrdersSpec extends MatcherSuiteBase with RawHttpChecks {

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

  "POST /matcher/orderbook/cancel" - {
    "should cancel all orders" in {

      val orders = Set(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
        mkOrder(alice, wavesUsdPair, SELL, 10.waves, 20.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
      )

      orders.foreach(placeAndAwaitAtDex(_))

      val r = validate200Json(dex1.rawApi.cancelAll(alice))

      r.success should be(true)
      r.status should be("BatchCancelCompleted")
      r.message.head should have size orders.size

      r.message.foreach(m => {
        m.foreach {
          case util.Right(HttpSuccessfulSingleCancel(_, success, status)) => success should be(true); status should be("OrderCanceled")
          case _ => fail(s"Unexpected response $r")
        }
      })

      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
    }

    "should return OK if there is nothing to cancel" in {
      validate200Json(dex1.rawApi.cancelAll(alice))
    }

    "should return an error if publicKey parameter has the different value of used in signature" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      validateIncorrectSignature(dex1.rawApi.cancelAll(bob, ts, sign))
    }

    "should return an error if timestamp header has the different value of used in signature" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      validateIncorrectSignature(dex1.rawApi.cancelAll(alice, ts + 1000, sign))
    }
  }

}
