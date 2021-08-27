package com.wavesplatform.it.matcher.api.http.history

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderBookHistoryItem
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.error.InvalidAddress
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.{toHttpOrderBookHistoryItem, ApiKeyHeaderChecks}

class GetOrderHistoryByAddressWithKeySpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

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

  "GET /matcher/orders/{address}" - {
    "should return all order history" in {

      withClue("empty history") {
        validate200Json(dex1.rawApi.getOrderHistoryByAddressWithKey(alice.stringRepr)) should have size 0
      }

      val orders = List(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
        mkOrder(alice, wavesUsdPair, BUY, 11.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 12.waves, 3.usd)
      )

      val historyActive = orders.map { order =>
        placeAndAwaitAtDex(order)
        toHttpOrderBookHistoryItem(order, OrderStatus.Accepted)
      }.sorted(HttpOrderBookHistoryItem.httpOrderBookHistoryItemOrdering)

      withClue("active only") {
        validate200Json(
          dex1.rawApi.getOrderHistoryByAddressWithKey(
            alice.stringRepr,
            activeOnly = true,
            closedOnly = false,
            Map("X-API-KEY" -> apiKey)
          )
        ) should matchTo(historyActive)
      }

      withClue("without params") {
        validate200Json(dex1.rawApi.getOrderHistoryByAddressWithKey(alice.stringRepr)) should matchTo(historyActive)
      }

      val historyCancelled = orders.map { order =>
        cancelAndAwait(alice, order)
        toHttpOrderBookHistoryItem(order, OrderStatus.Cancelled(0, 0))
      }.sorted(HttpOrderBookHistoryItem.httpOrderBookHistoryItemOrdering)

      withClue("closed only") {
        validate200Json(
          dex1.rawApi.getOrderHistoryByAddressWithKey(
            alice.stringRepr,
            activeOnly = false,
            closedOnly = true,
            Map("X-API-KEY" -> apiKey)
          )
        ) should matchTo(historyCancelled)
      }

    }

    "should return an error if address is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderHistoryByAddressWithKey("null"),
        StatusCode.BadRequest,
        InvalidAddress.code,
        "Provided address is not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error if address has an incorrect length" in {
      validateMatcherError(
        dex1.rawApi.getOrderHistoryByAddressWithKey("AAAAA"),
        StatusCode.BadRequest,
        InvalidAddress.code,
        "Provided address is not correct, reason: Wrong addressBytes length: expected: 26, actual: 4"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getOrderHistoryByAddressWithKey(
      alice.stringRepr,
      activeOnly = false,
      closedOnly = true,
      Map.empty
    ))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getOrderHistoryByAddressWithKey(
      alice.stringRepr,
      activeOnly = false,
      closedOnly = true,
      incorrectApiKeyHeader
    ))
  }

}
