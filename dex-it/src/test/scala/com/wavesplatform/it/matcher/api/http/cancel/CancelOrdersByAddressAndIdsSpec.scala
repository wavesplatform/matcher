package com.wavesplatform.it.matcher.api.http.cancel

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulSingleCancel
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class CancelOrdersByAddressAndIdsSpec extends MatcherSuiteBase with RawHttpChecks {

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

  private def placeAndGetIds(): Set[String] =
    Set(
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
    ).map { o =>
      placeAndAwaitAtDex(o)
      o.idStr()
    }

  "POST /matcher/orders/{address}/cancel" - {
    "should cancel orders by ids" in {

      val orders = Set(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
      )

      val ids = orders.map { o =>
        placeAndAwaitAtDex(o)
        o.idStr()
      }

      val r = validate200Json(dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, ids))

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
      validate200Json(dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, Set.empty))
    }

    "should return an error when one of ids is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, placeAndGetIds() + "null"),
        StatusCodes.BadRequest,
        1048577,
        "The provided JSON contains invalid fields: (3). Check the documentation"
      )
    }

    "should return an error when address is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelAllByAddressAndIds("null", placeAndGetIds()),
        StatusCodes.BadRequest,
        4194304,
        "Provided address in not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error when without headers" in {
      validateAuthorizationError(
        dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, placeAndGetIds(), Map.empty)
      )
    }

    "should return an error when the public api-key header is not correct" in {
      validateAuthorizationError(
        dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, placeAndGetIds(), Map("X-API-Key" -> "incorrect"))
      )
    }
  }

}
