package com.wavesplatform.it.matcher.api.http

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderBookHistoryItem
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.model.AcceptedOrderType
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderStatusInfoByIdWithApiKeySpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

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
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should be(HttpOrderBookHistoryItem(
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

        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should be(HttpOrderBookHistoryItem(
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
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should be(HttpOrderBookHistoryItem(
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
        validate200Json(dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, o.id(), Some(alice.publicKey))) should be(HttpOrderBookHistoryItem(
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

    "should return an error when the public key header is not of order owner" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(order)
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, order.id(), Some(bob.publicKey)),
        StatusCodes.Forbidden,
        3148801,
        "Provided user public key is not correct"
      )
    }

    "should return an error when the order doesn't exist" in {
      val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      validateMatcherError(
        dex1.rawApi.getOrderStatusInfoByIdWithApiKey(alice, order.id(), Some(alice.publicKey)),
        StatusCodes.NotFound,
        9437193,
        s"The order ${order.idStr()} not found"
      )
    }

  }

}
