package com.wavesplatform.it.sync.compat

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.waves.WavesFeeConstants.matcherFee
import com.wavesplatform.it.api.MatcherCommand
import com.wavesplatform.it.tags.DexMultipleVersions
import com.wavesplatform.it.{executeCommands, executePlaces, orderGen}
import org.scalacheck.Gen
import org.testcontainers.containers.BindMode

import scala.concurrent.duration.DurationInt
import scala.util.Random

@DexMultipleVersions
class DatabaseBackwardCompatTestSuite extends BackwardCompatSuiteBase {

  val assetPairs = List(ethWavesPair, wavesUsdPair)
  val additionalAssetPair = btcUsdnPair

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

    markup("cancel one order at DEX2")
    val orderToCancel = orders.head
    val cancelOrderCommand = MatcherCommand.CancelByOrderId(dex2, orderToCancel)
    executeCommands(Seq(cancelOrderCommand), ignoreErrors = false, timeout = 5.second) // it must be short operation
    dex2.api.waitForOrderStatus(orderToCancel, Status.Cancelled)

    markup("place order for additional asset pair at DEX2")
    val ts = System.currentTimeMillis()
    val additionalOrder = Order.buy(
      alice,
      matcher,
      additionalAssetPair,
      50,
      50 * Order.PriceConstant,
      ts,
      ts + 6000000,
      matcherFee,
      2
    )
    executeCommands(Seq(MatcherCommand.Place(dex2, additionalOrder)), timeout = 5.second) // it must be short operation
    dex2.api.waitForOrder(additionalOrder)(_.status != Status.NotFound)

    markup("delete orderbook for additional asset pair at DEX2")
    executeCommands(Seq(MatcherCommand.DeleteOrderBook(dex2, additionalAssetPair)), timeout = 5.second) // it must be short operation

    Thread.sleep(2.minutes.toMillis) // An additional time to wait the concurrent processing
    val state2 = state(dex2.api, orders)

    dex2.stopWithoutRemove()
    dex1.start()

    orders.groupBy(_.assetPair).valuesIterator.foreach { orders =>
      dex1.api.waitForOrder(orders.last)(_.status != Status.NotFound)
    }
    Thread.sleep(2.minutes.toMillis) // An additional time to wait the concurrent processing
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
