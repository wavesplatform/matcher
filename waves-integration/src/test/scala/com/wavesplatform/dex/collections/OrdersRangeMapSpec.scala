package com.wavesplatform.dex.collections

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

class OrdersRangeMapSpec extends WavesIntegrationSuiteBase with Matchers {

  "OrdersRangeMap" - {

    val order1 = sample
    val order2 = sample
    val order3 = sample

    val map =
      OrdersRangeMap
        .empty
        .updated(price = 5L, order1)
        .updated(price = 5L, order2)
        .updated(price = 7L, order3)

    "contains" in {
      map.contains(id = ByteStr.empty) shouldBe false
      map.contains(order1.id()) shouldBe true
      map.contains(order2.id()) shouldBe true
      map.contains(order3.id()) shouldBe true
    }

    "get" in {
      map.get(id = ByteStr.empty) shouldBe Option.empty[Order]
      map.get(order1.id()) shouldBe Option(order1)
      map.get(order2.id()) shouldBe Option(order2)
      map.get(order3.id()) shouldBe Option(order3)
    }

    "range (identity)" in {
      map.range(price = 3L) shouldBe Vector.empty[Order]
      map.range(price = 5L) shouldBe Vector(order1, order2)
    }

    "range" in {
      map.range(begin = 1L, end = 3L) shouldBe Vector.empty[Order]
      map.range(begin = 3L, end = 9L) shouldBe Vector(order1, order2, order3)
      map.range(begin = 9L, end = 3L) shouldBe Vector(order3, order1, order2)
      map.range(begin = 5L, end = 7L) shouldBe Vector(order3)
      map.range(begin = 7L, end = 5L) shouldBe Vector(order1, order2)
    }

    "removed" in {
      map.removed(id = ByteStr.empty) shouldBe map

      val mapWithoutOrder1 = map.removed(order1.id())
      map.contains(order1.id()) shouldBe true
      mapWithoutOrder1.contains(order1.id()) shouldBe false

      val mapWithoutOrder2 = mapWithoutOrder1.removed(order2.id())
      mapWithoutOrder1.contains(order2.id()) shouldBe true
      mapWithoutOrder2.contains(order2.id()) shouldBe false

      val mapWithoutOrder3 = mapWithoutOrder2.removed(order3.id())
      mapWithoutOrder2.contains(order3.id()) shouldBe true
      mapWithoutOrder3.contains(order3.id()) shouldBe false
    }

    "removedAll (identity)" in {
      map.removedAll(price = 3L) shouldBe map

      val mapWithoutRemovedOrders = map.removedAll(price = 5L)
      map.contains(order1.id()) shouldBe true
      map.contains(order2.id()) shouldBe true
      mapWithoutRemovedOrders.contains(order1.id()) shouldBe false
      mapWithoutRemovedOrders.contains(order2.id()) shouldBe false
    }

    "removedAll" in {
      map.removedAll(begin = 1L, end = 3L) shouldBe map

      val mapWithoutRemovedOrders1 = map.removedAll(begin = 3L, end = 9L)
      map.contains(order1.id()) shouldBe true
      map.contains(order2.id()) shouldBe true
      map.contains(order3.id()) shouldBe true
      mapWithoutRemovedOrders1.contains(order1.id()) shouldBe false
      mapWithoutRemovedOrders1.contains(order2.id()) shouldBe false
      mapWithoutRemovedOrders1.contains(order3.id()) shouldBe false

      val mapWithoutRemovedOrders2 = map.removedAll(begin = 9L, end = 3L)
      map.contains(order3.id()) shouldBe true
      map.contains(order1.id()) shouldBe true
      map.contains(order2.id()) shouldBe true
      mapWithoutRemovedOrders2.contains(order3.id()) shouldBe false
      mapWithoutRemovedOrders2.contains(order1.id()) shouldBe false
      mapWithoutRemovedOrders2.contains(order2.id()) shouldBe false

      val mapWithoutRemovedOrders3 = map.removedAll(begin = 5L, end = 7L)
      map.contains(order1.id()) shouldBe true
      map.contains(order2.id()) shouldBe true
      map.contains(order3.id()) shouldBe true
      mapWithoutRemovedOrders3.contains(order1.id()) shouldBe true
      mapWithoutRemovedOrders3.contains(order2.id()) shouldBe true
      mapWithoutRemovedOrders3.contains(order3.id()) shouldBe false

      val mapWithoutRemovedOrders4 = map.removedAll(begin = 7L, end = 5L)
      map.contains(order3.id()) shouldBe true
      map.contains(order1.id()) shouldBe true
      map.contains(order2.id()) shouldBe true
      mapWithoutRemovedOrders4.contains(order3.id()) shouldBe true
      mapWithoutRemovedOrders4.contains(order1.id()) shouldBe false
      mapWithoutRemovedOrders4.contains(order2.id()) shouldBe false
    }

  }

  def sample: Order =
    Gen.alphaStr
      .map { seed =>
        KeyPair(
          crypto.secureHash(
            Bytes.concat(
              Ints.toByteArray(0),
              seed.getBytes(StandardCharsets.UTF_8)
            )
          )
        )
      }
      .map { sender =>
        Order.buy(
          sender,
          matcher = PublicKey.empty,
          pair = AssetPair(IssuedAsset(ByteStr.empty), IssuedAsset(ByteStr.empty)),
          amount = 0L,
          price = 0L,
          timestamp = System.currentTimeMillis(),
          expiration = System.currentTimeMillis() + 600000L,
          matcherFee = 300000L,
          version = 3
        )
      }
      .sample
      .get

}
