package com.wavesplatform.it.matcher.api.http.markets

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class DeleteOrderBookSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

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

      validate202Json(dex1.rawApi.deleteOrderBook(wavesUsdPair)).message should be("Deleting order book")

      eventually {
        dex1.api.waitForOrderStatus(order, Status.Cancelled)
      }
    }

    //TODO: DEX-986
    "should return an error if orderbook doesn't exists" ignore {
      validateMatcherError(
        dex1.rawApi.deleteOrderBook(wavesUsdPair),
        StatusCodes.BadRequest,
        8388609,
        s"The order book for WAVES-$UsdId is unavailable, please contact with the administrator"
      )
    }

    //TODO: change after DEX-980
    "should return an error exception when the amount asset is not correct base58 string" in {
      validate404Exception(dex1.rawApi.deleteOrderBook("null", UsdId.toString, Map("X-API-KEY" -> apiKey)))
    }

    //TODO: change after DEX-980
    "should return an error exception when the price asset is not correct base58 string" in {
      validate404Exception(dex1.rawApi.deleteOrderBook("WAVES", "null", Map("X-API-KEY" -> apiKey)))
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.deleteOrderBook("WAVES", UsdId.toString, Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.deleteOrderBook("WAVES", UsdId.toString, incorrectApiKeyHeader))
  }

}
