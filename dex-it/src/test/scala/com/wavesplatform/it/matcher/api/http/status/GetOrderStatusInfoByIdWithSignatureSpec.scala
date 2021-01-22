package com.wavesplatform.it.matcher.api.http.status

import com.google.common.primitives.Longs
import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderBookHistoryItem
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.model.AcceptedOrderType
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderStatusInfoByIdWithSignatureSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{publicKey}/{orderId} " - {

    "should return correct status of the order" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)

      withClue(" - accepted") {
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithSignature(alice, o)) should matchTo(HttpOrderBookHistoryItem(
          o.id(),
          BUY,
          AcceptedOrderType.Limit,
          10.waves,
          0,
          2.usd,
          0.003.waves,
          0,
          o.feeAsset,
          o.timestamp,
          Status.Accepted.name,
          wavesUsdPair,
          0,
          o.version,
          0
        ))
      }

      withClue(" - partially filled") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 2.usd))

        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithSignature(alice, o)) should matchTo(HttpOrderBookHistoryItem(
          o.id(),
          BUY,
          AcceptedOrderType.Limit,
          10.waves,
          5.waves,
          2.usd,
          0.003.waves,
          0.0015.waves,
          o.feeAsset,
          o.timestamp,
          Status.PartiallyFilled.name,
          wavesUsdPair,
          2.usd,
          o.version,
          10.usd
        ))
      }

      withClue(" - filled") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 2.usd))
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithSignature(alice, o)) should matchTo(HttpOrderBookHistoryItem(
          o.id(),
          BUY,
          AcceptedOrderType.Limit,
          10.waves,
          10.waves,
          2.usd,
          0.003.waves,
          0.003.waves,
          o.feeAsset,
          o.timestamp,
          Status.Filled.name,
          wavesUsdPair,
          2.usd,
          o.version,
          20.usd
        ))
      }

      withClue(" - cancelled") {
        val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
        placeAndAwaitAtDex(o)
        cancelAndAwait(alice, o)
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithSignature(alice, o)) should matchTo(HttpOrderBookHistoryItem(
          o.id(),
          BUY,
          AcceptedOrderType.Limit,
          10.waves,
          0,
          2.usd,
          0.003.waves,
          0,
          o.feeAsset,
          o.timestamp,
          Status.Cancelled.name,
          wavesUsdPair,
          0,
          o.version,
          0
        ))
      }
    }

    "should return an error when the order doesn't exist" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithSignature(alice, order),
        StatusCodes.NotFound,
        9437193,
        s"The order ${order.idStr()} not found"
      )
    }

    "should return an error exception when the publicKey is not correct base58 string" in {
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithSignature("null", "null", ts, sign),
        StatusCodes.BadRequest,
        9437185,
        s"Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error exception when the orderId is not correct base58 string" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithSignature(Base58.encode(alice.publicKey), "null", ts, sign),
        StatusCodes.BadRequest,
        9437185,
        s"Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error if publicKey parameter has the different value of used in signature" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getOrderStatusInfoByIdWithSignature(Base58.encode(bob.publicKey), order.idStr(), ts, sign))
    }

    "should return an error if timestamp header has the different value of used in signature" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getOrderStatusInfoByIdWithSignature(Base58.encode(alice.publicKey), order.idStr(), ts + 1000, sign))
    }

    // DEX-982
    "should return an error timestamp header doesn't exist" ignore {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getOrderStatusInfoByIdWithSignature(
        Base58.encode(alice.publicKey),
        order.idStr(),
        Map("Signature" -> sign)
      ))
    }

    // DEX-982
    "should return an error signature header doesn't exist" ignore {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)

      validateIncorrectSignature(dex1.rawApi.getOrderStatusInfoByIdWithSignature(
        Base58.encode(alice.publicKey),
        order.idStr(),
        Map("Timestamp" -> System.currentTimeMillis.toString)
      ))
    }

    "should return an error with incorrect signature" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      validateIncorrectSignature(dex1.rawApi.getOrderStatusInfoByIdWithSignature(
        Base58.encode(alice.publicKey),
        order.idStr(),
        System.currentTimeMillis,
        "incorrect"
      ))
    }
  }
}
