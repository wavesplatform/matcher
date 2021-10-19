package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulPlace
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.concurrent.PatienceConfiguration.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.chaining._

final class NewOrdersRaceConditionTest extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |}""".stripMargin
    )

  "NewOrdersRaceConditionTest" - {
    "fail" in {
      val statuses = Set(Status.Filled, Status.Accepted, Status.PartiallyFilled)

      val now = System.currentTimeMillis()
      val carol = mkAccountWithBalance((defaultAssetQuantity / 3, usd), (500.waves, Waves))

      val copyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 5.waves, 27.usd, ts = now).tap(v => log.info(s"Copy order ${v.id()}"))

      val carolOrders = (1 to 25).map(i => mkOrder(carol, wavesUsdPair, OrderType.BUY, 1.waves, 1.usd, ts = now + i)) ++ (1 to 25).map(i =>
        mkOrder(carol, wavesUsdPair, OrderType.SELL, 1.waves, 20.usd, ts = now + i)
      )

      val counterOrders = (1 to 20).map(i => mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 19.usd, ts = now + i)) ++
        (0 to 20).map(_ => copyOrder)
      val submittedOrders = Seq(mkOrder(bob, wavesUsdPair, OrderType.SELL, 150000.waves, 25.usd, ts = now))

      Future.sequence(
        submittedOrders.map(dex1.asyncApi.place) ++ carolOrders.map(dex1.asyncApi.place) ++ counterOrders.map(dex1.asyncApi.place)
      ).recover {
        // not fail at order already placed error
        case p: RuntimeException if p.getMessage.contains("3148040") => HttpSuccessfulPlace(counterOrders.last)
      }.futureValue

      counterOrders.foreach(dex1.api.waitForOrder(_)(status => statuses.contains(status.status)))
      submittedOrders.foreach(dex1.api.waitForOrder(_)(status => statuses.contains(status.status)))

      eventually {
        submittedOrders.foreach { so =>
          dex1.api.getTransactionsByOrderId(so.id()).size > 1
        }
      }
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, bob, defaultAssetQuantity / 3, usd))
    dex1.start()
  }

}
