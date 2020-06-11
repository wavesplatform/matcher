package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.ApiOrderStatus
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.it.{MatcherSuiteBase, orderGen}
import org.scalacheck.Gen

class OrderBookSnapshotsTestSuite extends MatcherSuiteBase {
  private val interval = 50L

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
      |  price-assets = ["$UsdId", "WAVES"]
      |  snapshots-interval = $interval
      |}""".stripMargin
  )

  private val assetPair1 = ethUsdPair
  private val assetPair2 = ethWavesPair

  private val ordersPack1Size = 11
  private val ordersPack1 = Gen
    .containerOfN[Vector, Order](ordersPack1Size - 1, orderGen(matcher, alice, List(assetPair1)))
    .sample
    .get :+ orderGen(matcher, alice, List(assetPair2)).sample.get

  private val ordersPack2Size = interval.toInt
  private val ordersPack2 = Gen
    .containerOfN[Vector, Order](ordersPack2Size, orderGen(matcher, alice, List(assetPair2)))
    .sample
    .get

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueEthTx, IssueUsdTx)
    dex1.start()
  }

  "Order books are created with right offsets" in {

    ordersPack1.foreach(dex1.api.place)
    dex1.api.waitForCurrentOffset(_ == ordersPack1Size - 1)

    val allSnapshotOffsets1 = dex1.api.allSnapshotOffsets

    withClue("We doesn't show pairs, those have snapshot's offset equal to -1") {
      if (allSnapshotOffsets1.contains(assetPair1)) allSnapshotOffsets1(assetPair1) should be < interval
      if (allSnapshotOffsets1.contains(assetPair2)) allSnapshotOffsets1(assetPair2) should be < interval
    }

    ordersPack2.foreach(dex1.api.place)
    dex1.api.waitForCurrentOffset(_ == ordersPack1Size + ordersPack2Size - 1)

    val allSnapshotOffsets2 = dex1.api.allSnapshotOffsets

    withClue("Asset pairs has right offsets") {
      allSnapshotOffsets2.foreach {
        case (pair, offset) =>
          withClue(pair) {
            offset should be < (interval * 2)
          }
      }
    }
  }

  "All events are processed after restart" in {
    dex1.restart()
    dex1.api.waitForCurrentOffset(_ == ordersPack1Size + ordersPack2Size - 1)
    ordersPack1.foreach { order =>
      dex1.api.orderStatus(order) should not be ApiOrderStatus.Status.NotFound.name
    }
  }
}
