package com.wavesplatform.it.sync.compat

import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.it.api.MatcherCommand
import com.wavesplatform.it.tags.DexMultipleVersions
import com.wavesplatform.it.{executePlaces, orderGen}
import org.scalacheck.Gen
import org.testcontainers.containers.BindMode

import scala.concurrent.duration.DurationInt
import scala.util.Random

@DexMultipleVersions
class DatabaseBackwardCompatTestSuite extends BackwardCompatSuiteBase {

  val assetPairs = List(ethWavesPair, wavesUsdPair)
  val additionalAssetPair = ethUsdPair

  "Database backward compatibility test" in {

    val twoAccountsOrdersGen = Gen.oneOf(
      orderGen(matcher, alice, assetPairs),
      orderGen(matcher, bob, assetPairs)
    )

    markup("generate place commands for 100 limit and 100 market orders")
    // To ignore not important errors
    val placeCommands = Random.shuffle(
      Gen
        .containerOfN[Vector, Order](200, twoAccountsOrdersGen).sample
        .get
        .zipWithIndex
        .map(p => MatcherCommand.Place(dex2, p._1, (p._2 % 2) == 0)) // make half of it market orders
    )

    markup("place orders at DEX2")
    val orders = executePlaces(placeCommands)
    withClue("orders.size: ") {
      orders.size shouldBe >(100) // if its > 100, then there will be not only limit or place orders
    }

    orders.groupBy(_.assetPair).valuesIterator.foreach { orders =>
      dex2.api.waitForOrder(orders.last)(_.status != Status.NotFound)
    }

    markup("cancel orders in each pair at DEX2")
    val ordersToCancel = List(alice, bob)
      .flatMap(s => dex2.api.getOrderHistoryByPKWithSig(s, activeOnly = Some(true)).map((s, _)))
      .groupBy { case (_, order) => order.assetPair }
      .values
      .flatMap(_.headOption)
    ordersToCancel.foreach { case (owner, orderToCancel) =>
      dex2.api.cancelOneOrderWithKey(orderToCancel.id, Some(owner.publicKey))
      dex2.api.waitForOrderStatus(orderToCancel.assetPair, orderToCancel.id, Status.Cancelled)
    }

    markup("place order for additional asset pair at DEX2")
    val additionalOrder = mkOrder(alice, additionalAssetPair, OrderType.BUY, 50, 50 * Order.PriceConstant)
    dex2.api.place(additionalOrder)
    dex2.api.waitForOrder(additionalOrder)(_.status != Status.NotFound)

    markup("delete orderbook for additional asset pair at DEX2")
    dex2.restartWithNewSuiteConfig(
      dexInitialSuiteConfig.withFallback(ConfigFactory.parseString(s""" waves.dex.blacklisted-assets=["$UsdId"] """))
    )
    dex2.tryApi.deleteOrderBookWithKey(additionalAssetPair)
    dex2.restartWithNewSuiteConfig(dexInitialSuiteConfig)

    eventually {
      dex2.api.getAllSnapshotOffsets.contains(additionalAssetPair) shouldBe true
    }
    Thread.sleep(1.minute.toMillis) // An additional time to wait the concurrent processing

    val state2 = state(dex2.api, orders)

    markup("stop DEX2 and start DEX1")
    dex2.stopWithoutRemove()
    dex1.start()

    markup("wait for orders at DEX1")
    orders.groupBy(_.assetPair).valuesIterator.foreach { orders =>
      dex1.api.waitForOrder(orders.last)(_.status != Status.NotFound)
    }
    ordersToCancel.foreach { case (_, orderToCancel) =>
      dex1.api.waitForOrderStatus(orderToCancel.assetPair, orderToCancel.id, Status.Cancelled)
    }

    eventually {
      dex1.api.getAllSnapshotOffsets.contains(additionalAssetPair) shouldBe false
    }
    Thread.sleep(1.minute.toMillis) // An additional time to wait the concurrent processing

    markup("check DEX1 and DEX2 states")
    val state1 = state(dex1.api, orders)
    state1 should matchTo(state2)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val containerDataDir = "/opt/waves-dex/data"
    List(dex1, dex2).foreach {
      _.underlying.configure {
        _.withFileSystemBind(localLogsDir.resolve("db").toString, containerDataDir, BindMode.READ_WRITE)
      }
    }

    dex2.start()
  }

}
