package com.wavesplatform.it.matcher.api.http.status

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.{toHttpOrderBookHistoryItem, ApiKeyHeaderChecks}
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderStatusInfoByIdWithApiKeySpec extends MatcherSuiteBase with ApiKeyHeaderChecks with TableDrivenPropertyChecks {

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

  "GET /matcher/orders/{address}/{orderId} " - {

    "should return correct status of the order" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)

      withClue(" - accepted") {
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should matchTo(
          toHttpOrderBookHistoryItem(
            o,
            OrderStatus.Accepted
          )
        )
      }

      withClue(" - partially filled") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 2.usd))

        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should matchTo(
          toHttpOrderBookHistoryItem(
            o,
            OrderStatus.PartiallyFilled(5.waves, 0.0015.waves),
            avgWeighedPrice = 2.usd,
            totalExecutedPriceAssets = 10.usd
          )
        )
      }

      withClue(" - filled") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 2.usd))

        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should matchTo(
          toHttpOrderBookHistoryItem(
            o,
            OrderStatus.Filled(10.waves, 0.003.waves),
            avgWeighedPrice = 2.usd,
            totalExecutedPriceAssets = 20.usd
          )
        )
      }

      withClue(" - cancelled") {
        val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
        placeAndAwaitAtDex(o)
        cancelAndAwait(alice, o)

        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should matchTo(
          toHttpOrderBookHistoryItem(
            o,
            OrderStatus.Cancelled(0, 0)
          )
        )
      }
    }

    "should return an error when the public key header is not of order owner" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, order.id(), Some(bob.publicKey)),
        StatusCodes.Forbidden,
        3148801,
        "Provided public key is not correct, reason: invalid public key"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getOrderStatusInfoById(alice.toAddress.stringRepr, order.idStr(), Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getOrderStatusInfoById(
      alice.toAddress.stringRepr,
      order.idStr(),
      Map("X-API-KEY" -> "incorrect")
    ))

    "should return an error when the order doesn't exist" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, order.id(), Some(alice.publicKey)),
        StatusCodes.NotFound,
        9437193,
        s"The order ${order.idStr()} not found"
      )
    }

    "should return an error when address is not a correct base58 string" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoById(
          "null",
          order.idStr(),
          Map("X-User-Public-Key" -> Base58.encode(alice.publicKey), "X-API-Key" -> apiKey)
        ),
        StatusCodes.BadRequest,
        4194304,
        "Provided address in not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error when orderId is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoById(
          alice.toAddress.stringRepr,
          "null",
          Map("X-User-Public-Key" -> Base58.encode(alice.publicKey), "X-API-Key" -> apiKey)
        ),
        StatusCodes.BadRequest,
        9437185,
        "Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

  }
}
