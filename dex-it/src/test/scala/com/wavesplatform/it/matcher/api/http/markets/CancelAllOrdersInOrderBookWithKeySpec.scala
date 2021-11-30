package com.wavesplatform.it.matcher.api.http.markets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

import java.util.concurrent.atomic.AtomicLong
import scala.util.chaining._

class CancelAllOrdersInOrderBookWithKeySpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  "CancelAllOrdersInOrderBookWithKeySpec" - {

    "POST /matcher/orderbook/{amountAsset}/{priceAsset}/cancel should cancel all orders in pair" in {
      val ts = new AtomicLong(System.currentTimeMillis())
      0 until 5 foreach { _ =>
        List(
          mkOrder(alice, wavesUsdPair, SELL, 10.waves, 6.usd, ts = ts.incrementAndGet()),
          mkOrder(alice, wavesUsdPair, BUY, 10.waves, 5.usd, ts = ts.incrementAndGet()),
          mkOrder(bob, wavesUsdPair, SELL, 10.waves, 7.usd, ts = ts.incrementAndGet()),
          mkOrder(bob, wavesUsdPair, BUY, 10.waves, 4.usd, ts = ts.incrementAndGet())
        ).foreach(placeAndAwaitAtDex(_))
      }

      val aliceOrders = eventually {
        dex1.api.orderHistoryByAddressWithKey(alice).map(_.id).tap(_.size shouldBe 10)
      }

      val bobOrders = eventually {
        dex1.api.orderHistoryByAddressWithKey(bob).map(_.id).tap(_.size shouldBe 10)
      }

      dex1.api.cancelAllInOrderBookWithKey(wavesUsdPair)

      eventually {
        (aliceOrders ++ bobOrders).foreach { oid =>
          dex1.api.orderStatusByAssetPairAndId(wavesUsdPair, oid).status shouldBe HttpOrderStatus.Status.Cancelled
        }
      }
    }

    shouldReturnErrorWithoutApiKeyHeader(
      dex1.rawApi.cancelAllInOrderBookWithKey(wavesUsdPair.amountAssetStr, wavesUsdPair.priceAssetStr, headers = Map.empty)
    )
  }

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, bob, 10000.usd, usd))
    dex1.start()
  }

}
