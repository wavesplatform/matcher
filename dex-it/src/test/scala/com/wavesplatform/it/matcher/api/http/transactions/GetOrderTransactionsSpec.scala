package com.wavesplatform.it.matcher.api.http.transactions

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderTransactionsSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET /matcher/transactions/{orderId}" - {

    "should return empty list when order doesn't exists " in {
      validate200Json(dex1.rawApi.getTransactionsByOrder(mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd))) should be(empty)
    }

    "should return empty list when order doesn't have transactions" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      validate200Json(dex1.rawApi.getTransactionsByOrder(o)) should be(empty)

      dex1.api.cancelAll(alice)
    }

    "should return one transaction when order partially filled with another one" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      placeAndAwaitAtNode(mkOrder(bob, wavesUsdPair, SELL, 5.waves, 2.usd))

      val r = validate200Json(dex1.rawApi.getTransactionsByOrder(o))

      r should have size 1
      r.head.orders() should have size 2
      r.head.orders().get(0).id().toString should be(o.idStr())
      dex1.api.cancelAll(alice)
      dex1.api.cancelAll(bob)
    }

    "should return 2 transactions when order filled by another 2 orders" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      placeAndAwaitAtNode(mkOrder(bob, wavesUsdPair, SELL, 5.waves, 2.usd))
      placeAndAwaitAtNode(mkOrder(bob, wavesUsdPair, SELL, 5.waves, 2.usd))

      val r = validate200Json(dex1.rawApi.getTransactionsByOrder(o))

      r should have size 2
      r.foreach(_.orders().get(0).id().toString should be(o.idStr()))
    }

    "should return an error when orderId is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getTransactionsByOrder("null"),
        StatusCodes.BadRequest,
        9437185,
        "Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }
  }

}
