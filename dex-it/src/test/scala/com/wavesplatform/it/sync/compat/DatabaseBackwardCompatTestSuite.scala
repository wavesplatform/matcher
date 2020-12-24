package com.wavesplatform.it.sync.compat

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.it.api.MatcherCommand
import com.wavesplatform.it.tags.DexMultipleVersions
import com.wavesplatform.it.{executePlaces, orderGen}
import org.scalacheck.Gen
import org.testcontainers.containers.BindMode

import scala.concurrent.duration.DurationInt
import scala.util.Random

@DexMultipleVersions
class DatabaseBackwardCompatTestSuite extends BackwardCompatSuiteBase {
  "Database backward compatibility test" in {
    val assetPairs = List(ethWavesPair, wavesUsdPair)
    val twoAccountsOrdersGen = Gen.oneOf(
      orderGen(matcher, alice, assetPairs),
      orderGen(matcher, bob, assetPairs)
    )

    // To ignore not important errors
    val placeCommands = Random.shuffle(
      Gen
        .containerOfN[Vector, Order](200, twoAccountsOrdersGen).sample
        .get
        .map(MatcherCommand.Place(dex2, _))
    )
    val orders = executePlaces(placeCommands)
    withClue("orders.size: ") {
      orders.size shouldBe >(0)
    }

    orders.groupBy(_.assetPair).valuesIterator.foreach { orders =>
      dex2.api.waitForOrder(orders.last)(_.status != Status.NotFound)
    }
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
