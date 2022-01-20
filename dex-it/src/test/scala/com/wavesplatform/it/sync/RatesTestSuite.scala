package com.wavesplatform.it.sync

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpRates
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.PriceConstant
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.error.FeeNotEnough
import com.wavesplatform.it.MatcherSuiteBase

class RatesTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      default {
       |        mode = dynamic
       |        dynamic {
       |          base-maker-fee = ${0.003.waves}
       |          base-taker-fee = ${0.003.waves}
       |        }
       |      }
       |      discount {
       |        asset = "$BtcId"
       |        value = 0
       |      }
       |    }
       |  }
       |}""".stripMargin
  )

  val defaultRateMap: HttpRates = Map(Waves -> 1d)

  val wctRate = 0.2
  val wctRateUpdated = 0.5

  val wctStr: String = WctId.toString
  val btcStr: String = BtcId.toString

  val (amount, price) = (1000L, PriceConstant)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueBtcTx)
    broadcastAndAwait(mkTransfer(bob, alice, matcherFee * 5, btc))
    dex1.start()
  }

  private def newOrder: Order = mkOrder(alice, wctUsdPair, BUY, amount, price, matcherFee = 300000, feeAsset = btc)

  "Changing rates affects order validation" in {
    // set rate for btc
    dex1.httpApi.upsertAssetRate(btc, 1).code shouldBe StatusCode.Created

    // place order with admissible fee (according to btc rate = 1)
    placeAndAwaitAtDex(newOrder)

    // slightly increase rate for btc
    dex1.httpApi.upsertAssetRate(btc, 1.2).code shouldBe StatusCode.Ok

    // the same order is passed, because we choose the minimal rate between 1 and 1.1
    placeAndAwaitAtDex(newOrder)

    // now a new order doesn't match both rates
    dex1.httpApi.upsertAssetRate(btc, 1.1).code shouldBe StatusCode.Ok

    // the same order now is rejected
    dex1.tryApi.place(newOrder) should failWith(
      FeeNotEnough.code,
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )

    // return previous rate for btc
    dex1.httpApi.upsertAssetRate(btc, 1).code shouldBe StatusCode.Ok

    placeAndAwaitAtDex(newOrder)

    dex1.api.deleteAssetRate(btc)
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add high rate for btc
    dex1.httpApi.upsertAssetRate(btc, 1.1).code shouldBe StatusCode.Created

    // order with low fee should be rejected
    dex1.tryApi.place(newOrder) should failWith(
      FeeNotEnough.code,
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )

    // restart matcher
    dex1.restart()

    // order with low fee should be rejected again
    dex1.tryApi.place(newOrder) should failWith(
      FeeNotEnough.code,
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )
  }
}
