package com.wavesplatform.it.sync

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class RatesTestSuite extends MatcherSuiteBase {
  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee {
       |    mode = dynamic
       |    dynamic {
       |      base-fee = 300000
       |    }
       |  }
       |}""".stripMargin
  )

  val defaultRateMap: Map[Asset, Double] = Map(Waves -> 1d)

  val wctRate        = 0.2
  val wctRateUpdated = 0.5

  val wctStr   = WctId.toString
  val wctAsset = IssuedAsset(WctId)

  val btcStr   = BtcId.toString
  val btcAsset = IssuedAsset(BtcId)

  val usdStr = UsdId.toString

  val (amount, price) = (1000L, PriceConstant)

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueBtcTx)
    broadcastAndAwait(mkTransfer(bob, alice, matcherFee * 5, btcAsset))
    startAndWait(dex1Container(), dex1Api)
  }

  private def newOrder: Order = mkOrder(alice, wctUsdPair, BUY, amount, price, matcherFee = 300000, matcherFeeAssetId = btcAsset)

  "Rates can be handled via REST" in {
    // default rates
    dex1Api.rates shouldBe defaultRateMap

    // add rate for unexisted asset
    dex1Api.tryUpsertRate(IssuedAsset(ByteStr.decodeBase58("unexistedAsset").get), 0.2) should failWith(
      11534345,
      MatcherError.Params(assetId = Some("unexistedAsset"))
    )

    // add rate for wct
    val addWctRate = dex1Api.upsertRate(wctAsset, wctRate)
    addWctRate._1 shouldBe StatusCodes.Created
    addWctRate._2.message shouldBe s"The rate $wctRate for the asset $wctStr added"
    dex1Api.rates shouldBe defaultRateMap + (wctAsset -> wctRate)

    // update rate for wct
    val updateWctRate = dex1Api.upsertRate(wctAsset, wctRateUpdated)
    updateWctRate._1 shouldBe StatusCodes.Ok
    updateWctRate._2.message shouldBe s"The rate for the asset $wctStr updated, old value = $wctRate, new value = $wctRateUpdated"
    dex1Api.rates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // update rate for Waves is not allowed
    dex1Api.tryUpsertRate(Waves, wctRateUpdated) should failWith(20971531, "The rate for WAVES cannot be changed")
    dex1Api.rates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // delete rate for wct
    dex1Api.deleteRate(wctAsset).message shouldBe s"The rate for the asset $wctStr deleted, old value = $wctRateUpdated"
    dex1Api.rates shouldBe defaultRateMap

    // delete unexisted rate
    dex1Api.tryDeleteRate(wctAsset) should failWith(20971529, MatcherError.Params(assetId = Some(wctStr)))
  }

  "Rates should not be changed by incorrect values" in {
    dex1Api.tryUpsertRate(Waves, 0) should failWith(20971535, "Asset rate should be positive")
    dex1Api.rates shouldBe defaultRateMap

    dex1Api.tryUpsertRate(Waves, -0.1) should failWith(20971535, "Asset rate should be positive")
    dex1Api.rates shouldBe defaultRateMap
  }

  "Changing rates affects order validation" in {
    // set rate for btc
    dex1Api.upsertRate(btcAsset, 1)._1 shouldBe StatusCodes.Created

    // place order with admissible fee (according to btc rate = 1)
    val order1 = newOrder
    placeAndAwait(order1)

    // slightly increase rate for btc
    dex1Api.upsertRate(btcAsset, 1.1)._1 shouldBe StatusCodes.Ok

    // the same order now is rejected
    dex1Api.tryPlace(newOrder) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )

    // return previous rate for btc
    dex1Api.upsertRate(btcAsset, 1)._1 shouldBe StatusCodes.Ok

    placeAndAwait(newOrder)

    dex1Api.deleteRate(btcAsset)
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add high rate for btc
    dex1Api.upsertRate(btcAsset, 1.1)._1 shouldBe StatusCodes.Created

    // order with low fee should be rejected
    dex1Api.tryPlace(newOrder) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )

    // restart matcher
    restartContainer(dex1Container(), dex1Api)

    // order with low fee should be rejected again
    dex1Api.tryPlace(newOrder) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr"
    )
  }
}
