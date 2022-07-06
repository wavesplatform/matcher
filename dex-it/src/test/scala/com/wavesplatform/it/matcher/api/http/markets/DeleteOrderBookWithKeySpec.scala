package com.wavesplatform.it.matcher.api.http.markets

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.error.InvalidAsset
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class DeleteOrderBookWithKeySpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "DELETE /matcher/orderbook/{amountAsset}/{priceAsset}" - {

    "should remove order book" in {
      val order = mkOrder(alice, wavesUsdPair, SELL, 10.waves, 1.usd)
      placeAndAwaitAtDex(order)

      dex1.restartWithNewSuiteConfig(ConfigFactory.parseString(
        s"""waves.dex {
           |  price-assets = [ "$UsdId", "WAVES" ]
           |  blacklisted-assets  = [$UsdId]
           |}""".stripMargin
      ))

      validate202Json(dex1.rawApi.deleteOrderBookWithKey(wavesUsdPair)).message should be("Deleting order book")
      eventually(dex1.api.getOrderHistoryByPKWithSig(alice, activeOnly = Some(true)).size shouldBe 0)
      dex1.api.getOrderHistoryByPKWithSig(alice, activeOnly = Some(false)).find(_.id == order.id()).value.status shouldBe "Cancelled"
    }

    "should return success if orderbook doesn't exists" in { // see DEX-1602
      validate202Json(dex1.rawApi.deleteOrderBookWithKey(wavesUsdPair)).message should be("Deleting order book")
    }

    "should return an error exception when the amount asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.deleteOrderBookWithKey("null", UsdId.toString, Map("X-API-KEY" -> apiKey)),
        StatusCode.BadRequest,
        InvalidAsset.code,
        s"The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error exception when the price asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.deleteOrderBookWithKey("WAVES", "null", Map("X-API-KEY" -> apiKey)),
        StatusCode.BadRequest,
        InvalidAsset.code,
        s"The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.deleteOrderBookWithKey("WAVES", UsdId.toString, Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.deleteOrderBookWithKey("WAVES", UsdId.toString, incorrectApiKeyHeader))
  }

}
