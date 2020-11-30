package com.wavesplatform.it.matcher.api.http.history

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.http.toHttpOrderBookHistoryItem
import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto

class GetOrderHistoryByPublicKeySpec extends MatcherSuiteBase with RawHttpChecks {

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

  "GET /matcher/orders/{publicKey}" - {
    "should return all order history" in {

      withClue("empty history") {
        validate200Json(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr)) should have size 0
      }

      val orders = List(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
        mkOrder(alice, wavesUsdPair, BUY, 11.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 12.waves, 3.usd)
      )

      val historyActive = orders.map { order =>
        placeAndAwaitAtDex(order)
        toHttpOrderBookHistoryItem(order, OrderStatus.Accepted)
      }.reverse

      withClue("active only") {
        validate200Json(
          dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr, true, false, Map("X-API-KEY" -> apiKey))
        ) should matchTo(historyActive)
      }

      withClue("without params") {
        validate200Json(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr)) should matchTo(historyActive)
      }

      val historyCancelled = orders.map { order =>
        cancelAndAwait(alice, order)
        toHttpOrderBookHistoryItem(order, OrderStatus.Cancelled(0, 0))
      }.reverse

      withClue("closed only") {
        validate200Json(
          dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr, false, true, Map("X-API-KEY" -> apiKey))
        ) should matchTo(historyCancelled)
      }

    }

    "should return an error if address is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderHistoryByApiKey("null"),
        StatusCodes.BadRequest,
        4194304,
        "Provided address in not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return and error without X-API-KEY" in {
      validateAuthorizationError(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr, false, true, Map.empty))
    }

    "should return and error with incorrect X-API-KEY" in {
      validateAuthorizationError(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr, false, true, Map("X-API-KEY" -> "incorrect")))
    }

    "should return an error if publicKey parameter has the different value of used in signature" in {
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getReservedBalance(Base58.encode(bob.publicKey), ts, sign))
    }

    "should return an error if timestamp header has the different value of used in signature" in {
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getReservedBalance(Base58.encode(alice.publicKey), ts + 1000, sign))
    }

    "should return an error with incorrect signature" in {
      validateIncorrectSignature(dex1.rawApi.getReservedBalance(Base58.encode(alice.publicKey), System.currentTimeMillis, "incorrect"))
    }
  }

}
