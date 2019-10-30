package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.OrderStatus
import com.wavesplatform.transaction.assets.exchange.OrderType

class OrderV3TestSuite extends MatcherSuiteBase {
  override protected val suiteInitialDexConfig: Config = allowedOrderVersion(1, 2)

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueUsdTx)
    startAndWait(dex1Container(), dex1Api)
  }

  "settings of allowing orderV3" - {
    val price = 100000000L

    "try to place not allowed orderV3" in {
      val orderV3 = mkOrder(alice, wavesUsdPair, OrderType.BUY, 3, price, version = 3)
      dex1Api.tryPlace(orderV3) should failWith(9439746, "The orders of version 3 are denied by matcher") // OrderVersionDenied
    }

    "matching orderV1 and orderV3" in {
      val orderV1 = mkOrder(alice, wavesUsdPair, OrderType.BUY, 3, price, version = 1)
      placeAndAwait(orderV1)

      replaceSuiteConfig(dex1Container(), allowedOrderVersion(1, 2, 3))
      restartContainer(dex1Container(), dex1Api)

      val orderV3 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 2, price, version = 3)
      dex1Api.place(orderV3)

      dex1Api.waitForOrderStatus(orderV1, OrderStatus.PartiallyFilled)
      dex1Api.waitForOrderStatus(orderV3, OrderStatus.Filled)
    }
  }

  private def allowedOrderVersion(versions: Int*): Config =
    ConfigFactory.parseString(s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  allowed-order-versions = [${versions.mkString(", ")}]
         |}""".stripMargin)
}
