package com.wavesplatform.it.matcher.api.http.cancel

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulSingleCancel
import com.wavesplatform.dex.domain.order.OrderType.BUY
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

      val r = validate200Json(dex1.rawApi.cancelOrdersByIdsWithKey(alice.toAddress.stringRepr, ids))

      r.success should be(true)
      r.status should be("BatchCancelCompleted")
      r.message.head should have size ids.size

      r.message.head.zipWithIndex.foreach { case (m, i) =>
        m match {
          case util.Right(HttpSuccessfulSingleCancel(id, success, status)) =>
            id.toString should be(ids(i))
            success should be(true)
            status should be("OrderCanceled")
            dex1.api.waitForOrderStatus(wavesUsdPair, id, Status.Cancelled)
          case _ => fail(s"Unexpected response $r")
        }
      }
    }

    "should return OK if there is nothing to cancel" in {
      validate200Json(dex1.rawApi.cancelOrdersByIdsWithKey(alice.stringRepr, Seq.empty[String]))
    }

    "should return an error when one of ids is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOrdersByIdsWithKey(alice.stringRepr, placeAndGetIds(3) :+ "null"),
        StatusCode.BadRequest,
        InvalidJson.code,
        "The provided JSON contains invalid fields: (3). Check the documentation"
      )
    }

    "should return an error when address is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOrdersByIdsWithKey("null", placeAndGetIds(3)),
        StatusCode.BadRequest,
        InvalidAddress.code,
        "Provided address is not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.cancelOneOrderWithKey(order.idStr(), headers = Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.cancelOneOrderWithKey(order.idStr(), incorrectApiKeyHeader))
  }

}
