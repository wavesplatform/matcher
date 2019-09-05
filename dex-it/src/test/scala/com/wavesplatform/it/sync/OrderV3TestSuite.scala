package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.{NTPTime, NewMatcherSuiteBase}
import com.wavesplatform.transaction.assets.exchange.OrderType

class OrderV3TestSuite extends NewMatcherSuiteBase with NTPTime {
  override protected val suiteInitialDexConfig: Config = allowedOrderVersion(1, 2)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueUsdTx)
  }

  "settings of allowing orderV3" - {
    val price = 100000000L

    "try to place not allowed orderV3" in {
      val orderV3 = mkOrder(alice, wavesUsdPair, OrderType.BUY, 3, price, version = 3)
      dex1Api.tryPlace(orderV3) should failWith(9439746, "The orders of version 3 are denied by matcher")
    }

    "matching orderV1 and orderV3" in {
      val orderV1 = mkOrder(alice, wavesUsdPair, OrderType.BUY, 3, price, version = 1)
      dex1Api.place(orderV1)
      dex1Api.waitForOrderStatus(orderV1, OrderStatus.Accepted)

      replaceSuiteConfig(dex1Container(), allowedOrderVersion(1, 2, 3))
      restartContainer(dex1Container(), dex1Api)

      val orderV3 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 2, price, version = 3)
      dex1Api.place(orderV3)

      dex1Api.waitForOrderStatus(orderV1, OrderStatus.PartiallyFilled)
      dex1Api.waitForOrderStatus(orderV3, OrderStatus.Filled)
    }
  }

  private def allowedOrderVersion(versions: Int*): Config =
    ConfigFactory.parseString(s"waves.dex.allowed-order-versions = [${versions.mkString(", ")}]")
}
