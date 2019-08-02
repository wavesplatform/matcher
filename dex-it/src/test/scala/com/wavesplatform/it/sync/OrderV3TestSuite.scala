package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.{MatcherSuiteBase, NTPTime}
import com.wavesplatform.transaction.assets.exchange.OrderType

import scala.concurrent.duration._

class OrderV3TestSuite extends MatcherSuiteBase with NTPTime {

  override protected def nodeConfigs: Seq[Config] = super.nodeConfigs.map(OrderV3TestSuite.matcherSettingsOrderV3Disabled.withFallback)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    node.waitForTransaction(node.broadcastRequest(IssueUsdTx.json()).id)
  }

  "settings of allowing orderV3" - {
    val price         = 100000000L

    "try to place not allowed orderV3" in {
      val orderv3 = node.prepareOrder(
        sender = alice,
        pair = wavesUsdPair,
        OrderType.BUY,
        amount = 3,
        price = price,
        version = 3,
      )
      assertBadRequestAndResponse(node.placeOrder(orderv3), "The orders of version 3 are denied by matcher")
    }

    "matching orderV1 and orderV3" in {
      val preparedOrderV2 = node.prepareOrder(
        sender = alice,
        pair = wavesUsdPair,
        OrderType.BUY,
        amount = 3,
        price = price
      )
      val order = node.placeOrder(preparedOrderV2)
      node.waitOrderProcessed(wavesUsdPair, order.message.id)

      docker.restartNode(node, OrderV3TestSuite.matcherSettingsOrderV3Allowed)
      val preparedOrderV3 = node.prepareOrder(
        sender = bob,
        pair = wavesUsdPair,
        OrderType.SELL,
        amount = 2,
        price = price,
        version = 3
      )
      val orderV3 = node.placeOrder(preparedOrderV3)
      node.waitOrderProcessed(wavesUsdPair, orderV3.message.id)

      node.waitOrderStatusAndAmount(wavesUsdPair, order.message.id, "PartiallyFilled", Some(2), 1.minute)
      node.waitOrderStatusAndAmount(wavesUsdPair, orderV3.message.id, "Filled", Some(2), 1.minute)
    }
  }

}

object OrderV3TestSuite {
  val matcherSettingsOrderV3Allowed: Config = ConfigFactory.parseString("waves.dex { allowed-order-versions = [1, 2, 3] }")
  val matcherSettingsOrderV3Disabled: Config = ConfigFactory.parseString("waves.dex { allowed-order = [1, 2] }")
}
