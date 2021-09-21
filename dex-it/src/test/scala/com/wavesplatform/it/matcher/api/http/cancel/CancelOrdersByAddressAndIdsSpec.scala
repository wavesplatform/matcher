package com.wavesplatform.it.matcher.api.http.cancel

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulSingleCancel
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{InvalidAddress, InvalidJson}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

import scala.util.Random

class CancelOrdersByAddressAndIdsSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

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

  val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

  "POST /matcher/orders/{address}/cancel" - {
    "should cancel orders by ids" in {
      val ids = Random.shuffle(placeAndGetIds(10))

      val r = validate200Json(dex1.rawApi.cancelOrdersByIdsWithKey(alice, ids))

      r.success should be(true)
      r.status should be("BatchCancelCompleted")
      r.message.head should have size ids.size

      r.message.head.zipWithIndex.foreach { case (m, i) =>
        m match {
          case util.Right(HttpSuccessfulSingleCancel(id, success, status)) =>
            id should be(ids(i))
            success should be(true)
            status should be("OrderCanceled")
            dex1.api.waitForOrderStatus(wavesUsdPair, id, Status.Cancelled)
          case _ => fail(s"Unexpected response $r")
        }
      }
    }

    "should cancel correct orders even if there are incorrect in list" in {
      val accepted = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
      val filledAlice = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      val partiallyFilled = mkOrder(alice, wavesUsdPair, BUY, 8.waves, 2.usd)
      val filledBob = mkOrder(bob, wavesUsdPair, SELL, 17.waves, 2.usd)
      val cancelled = mkOrder(alice, wavesUsdPair, BUY, 14.waves, 1.usd)

      Set(accepted, filledAlice).foreach(placeAndAwaitAtDex(_))
      placeAndAwaitAtDex(cancelled)
      placeAndAwaitAtNode(filledBob)
      placeAndAwaitAtNode(partiallyFilled)
      dex1.api.cancelOrderById(cancelled)

      val uniqueIds = Seq(accepted, filledAlice, partiallyFilled, filledBob, cancelled).map(o => o.id())

      val r = validate200Json(dex1.rawApi.cancelOrdersByIdsWithKey(alice, Random.shuffle(uniqueIds ++ uniqueIds)))
      r.success should be(true)
      r.status should be("BatchCancelCompleted")
      r.message.head should have size uniqueIds.size

      dex1.api.waitForOrderStatus(filledAlice, Status.Filled)
      dex1.api.waitForOrderStatus(filledBob, Status.Filled)
      Seq(accepted, partiallyFilled, cancelled).foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
    }

    "should return OK if there is nothing to cancel" in {
      validate200Json(dex1.rawApi.cancelOrdersByIdsWithKey(alice, Seq.empty[Order.Id]))
    }

    "should return an error when one of ids is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOrdersByIdsWithKey(placeAndGetIds(3).map(_.toString) :+ "null", alice.stringRepr),
        StatusCode.BadRequest,
        InvalidJson.code,
        "The provided JSON contains invalid fields: (3). Check the documentation"
      )
    }

    "should return an error when address is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOrdersByIdsWithKey(placeAndGetIds(3).map(_.toString), "null"),
        StatusCode.BadRequest,
        InvalidAddress.code,
        "Provided address is not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.cancelOneOrderWithKey(order.idStr(), headers = Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.cancelOneOrderWithKey(order.idStr(), incorrectApiKeyHeader))
  }

}
