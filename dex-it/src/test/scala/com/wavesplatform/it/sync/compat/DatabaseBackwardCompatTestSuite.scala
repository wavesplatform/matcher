package com.wavesplatform.it.sync.compat

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.orderGen
import com.wavesplatform.it.tags.DexMultipleVersions
import org.scalacheck.Gen
import org.testcontainers.containers.BindMode

import scala.concurrent.duration.DurationInt

@DexMultipleVersions
class DatabaseBackwardCompatTestSuite extends BackwardCompatSuiteBase {
  "Database backward compatibility test" in {
    val assetPairs = List(ethWavesPair, wavesUsdPair)
    val twoAccountsOrdersGen = Gen.oneOf(
      orderGen(matcher, alice, assetPairs),
      orderGen(matcher, bob, assetPairs)
    )

    val orders = Gen.containerOfN[Vector, Order](200, twoAccountsOrdersGen).sample.get
    orders.foreach(dex2.api.place)

    orders.groupBy(_.assetPair).valuesIterator.foreach { orders =>
      dex2.api.waitForOrder(orders.last)(_.status != Status.NotFound)
    }
    Thread.sleep(10.seconds.toMillis) // An additional time to wait the concurrent processing
    val state2 = state(dex2.api, orders)

    dex2.stopWithoutRemove()
    dex1.start()

    orders.groupBy(_.assetPair).valuesIterator.foreach { orders =>
      dex1.api.waitForOrder(orders.last)(_.status != Status.NotFound)
    }
    Thread.sleep(10.seconds.toMillis) // An additional time to wait the concurrent processing
    val state1 = state(dex1.api, orders)
    state1 should matchTo(state2)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val containerDataDir = DexContainer.containerPath("data")
    List(dex1, dex2).foreach {
      _.underlying.configure {
        _.withFileSystemBind(localLogsDir.resolve("db").toString, containerDataDir, BindMode.READ_WRITE)
      }
    }

    dex2.start()
  }
}
