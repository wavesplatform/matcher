package com.wavesplatform.it.sync

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpRates
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.PriceConstant
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.it.MatcherSuiteBase

class RatesTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
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

  "Rates can be handled via REST" in {
    // default rates
    dex1.api.rates shouldBe defaultRateMap

    // add rate for unexisted asset
    dex1.api.tryUpsertRate(IssuedAsset(ByteStr.decodeBase58("unexistedAsset").get), 0.2) should failWith(
      11534345,
      MatcherError.Params(assetId = Some("unexistedAsset"))
    )

    // add rate for wct
    val addWctRate = dex1.api.upsertRate(wct, wctRate)
    addWctRate._1 shouldBe StatusCodes.Created
    addWctRate._2.message shouldBe s"The rate $wctRate for the asset $wctStr added"
    dex1.api.rates shouldBe defaultRateMap + (wct -> wctRate)

    // update rate for wct
    val updateWctRate = dex1.api.upsertRate(wct, wctRateUpdated)
    updateWctRate._1 shouldBe StatusCodes.Ok
    updateWctRate._2.message shouldBe s"The rate for the asset $wctStr updated, old value = $wctRate, new value = $wctRateUpdated"
    dex1.api.rates shouldBe defaultRateMap + (wct -> wctRateUpdated)

    // update rate for Waves is not allowed
    dex1.api.tryUpsertRate(Waves, wctRateUpdated) should failWith(20971531, "The rate for WAVES cannot be changed")
    dex1.api.rates shouldBe defaultRateMap + (wct -> wctRateUpdated)

    // delete rate for wct
    dex1.api.deleteRate(wct).message shouldBe s"The rate for the asset $wctStr deleted, old value = $wctRateUpdated"
    dex1.api.rates shouldBe defaultRateMap

    // delete unexisted rate
    dex1.api.tryDeleteRate(wct) should failWith(20971529, MatcherError.Params(assetId = Some(wctStr)))
  }

  "Rates should not be changed by incorrect values" in {
    dex1.api.tryUpsertRate(Waves, 0) should failWith(20971535, "Asset rate should be positive")
    dex1.api.rates shouldBe defaultRateMap

    dex1.api.tryUpsertRate(Waves, -0.1) should failWith(20971535, "Asset rate should be positive")
    dex1.api.rates shouldBe defaultRateMap
  }

  "Changing rates affects order validation" in {
    // set rate for btc
    dex1.api.upsertRate(btc, 1)._1 shouldBe StatusCodes.Created

    // place order with admissible fee (according to btc rate = 1)
    val order1 = newOrder
    placeAndAwaitAtDex(order1)

    // slightly increase rate for btc
    dex1.api.upsertRate(btc, 1.1)._1 shouldBe StatusCodes.Ok

    // the same order now is rejected
    dex1.api.tryPlace(newOrder) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )

    // return previous rate for btc
    dex1.api.upsertRate(btc, 1)._1 shouldBe StatusCodes.Ok

    placeAndAwaitAtDex(newOrder)

    dex1.api.deleteRate(btc)
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add high rate for btc
    dex1.api.upsertRate(btc, 1.1)._1 shouldBe StatusCodes.Created

    // order with low fee should be rejected
    dex1.api.tryPlace(newOrder) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )

    // restart matcher
    dex1.restart()

    // order with low fee should be rejected again
    dex1.api.tryPlace(newOrder) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )
  }
}
