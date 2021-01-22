package com.wavesplatform.it.matcher.api.http.status

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import im.mak.waves.transactions.IssueTransaction
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderStatusSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/{orderId} " - {

    "should return correct status of the order" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

      withClue(" - not found") {
        validate200Json(dex1.rawApi.getOrderStatus(o)).status should be(Status.NotFound)
      }

      placeAndAwaitAtDex(o)

      withClue(" - accepted") {
        validate200Json(dex1.rawApi.getOrderStatus(o)).status should be(Status.Accepted)
      }

      withClue(" - partially filled") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 2.usd))
        validate200Json(dex1.rawApi.getOrderStatus(o)).status should be(Status.PartiallyFilled)
      }

      withClue(" - filled") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 2.usd))
        validate200Json(dex1.rawApi.getOrderStatus(o)).status should be(Status.Filled)
      }

      withClue(" - cancelled") {
        val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
        placeAndAwaitAtDex(o)
        cancelAndAwait(alice, o)
        validate200Json(dex1.rawApi.getOrderStatus(o)).status should be(Status.Cancelled)
      }
    }

    "should return an error exception when the amount asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderStatus("null", UsdId.toString, order.idStr()),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error exception when the price asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderStatus("WAVES", "null", order.idStr()),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error when amount asset doesn't exist" in {
      val incorrectAsset = "3Q6ndEq2z5UJwF4SF24ySRj9guPoFWaSeXP"

      validateMatcherError(
        dex1.rawApi.getOrderStatus(incorrectAsset, "WAVES", order.idStr()),
        StatusCodes.NotFound,
        11534345,
        s"The asset $incorrectAsset not found"
      )
    }

    "should redirect if price asset in base58 less than amount asset" in {

      def mkAsset(): IssueTransaction = {
        val tx = mkIssue(alice, "name", someAssetAmount, 2)
        if (tx.id().toString < "WAVES") tx
        else mkAsset()
      }

      val issuedAsset = mkAsset()
      broadcastAndAwait(issuedAsset)

      validate301Redirect(dex1.rawApi.getOrderStatus("WAVES", issuedAsset.assetId().toString, order.idStr()))
    }
  }
}
