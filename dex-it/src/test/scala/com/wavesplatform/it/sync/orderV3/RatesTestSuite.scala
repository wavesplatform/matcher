package com.wavesplatform.it.sync.orderV3

import akka.http.scaladsl.model.StatusCodes._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.sync.matcherFee
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.transfer.TransferTransactionV2

class RatesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val orderFeeSettingsStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = dynamic
         |    dynamic {
         |      base-fee = 300000
         |    }
         |  }  
         |}
       """.stripMargin

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  val defaultRateMap: Map[Asset, Double] = Map(Waves -> 1d)

  val wctRate = 0.2
  val wctRateUpdated = 0.5

  val wctStr = WctId.toString
  val wctAsset = IssuedAsset(WctId)

  val btcStr = BtcId.toString
  val btcAsset = IssuedAsset(BtcId)

  val usdStr = UsdId.toString

  val (amount, price) = (1000L, PriceConstant)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val txIds = Seq(IssueUsdTx, IssueWctTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))

    val transferTxId = node
      .broadcastRequest(
        TransferTransactionV2
          .selfSigned(
            assetId = btcAsset,
            sender = bob,
            recipient = alice.toAddress,
            amount = matcherFee * 5,
            timestamp = System.currentTimeMillis(),
            feeAssetId = Waves,
            feeAmount = 300000,
            attachment = Array.emptyByteArray
          )
          .explicitGet()
          .json())
      .id
    node.waitForTransaction(transferTxId)
  }

  def getOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, amount, price, fee = matcherFee, version = 3, feeAssetId = btcAsset)

  "Rates can be handled via REST" in {
    // default rates
    node.getRates shouldBe defaultRateMap

    // add rate for unexisted asset
    assertNotFoundAndMessage(node.upsertRate(IssuedAsset(ByteStr.decodeBase58("unexistedAsset").get), 0.2, expectedStatusCode = Created),
      "The asset unexistedAsset not found")

    // add rate for wct
    node.upsertRate(wctAsset, wctRate, expectedStatusCode = Created).message shouldBe s"Rate $wctRate for the asset $wctStr added"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRate)

    // update rate for wct
    node
      .upsertRate(wctAsset, wctRateUpdated, expectedStatusCode = OK)
      .message shouldBe s"Rate for the asset $wctStr updated, old value = $wctRate, new value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // update rate for Waves is not allowed
    node.upsertRate(Waves, wctRateUpdated, expectedStatusCode = BadRequest).message shouldBe "The rate for WAVES cannot be changed"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // delete rate for wct
    node.deleteRate(wctAsset).message shouldBe s"Rate for the asset $wctStr deleted, old value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap

    // delete unexisted rate
    assertNotFoundAndMessage(node.deleteRate(wctAsset), s"The rate for the asset $wctStr was not specified")
  }

  "Rates should not be changed by incorrect values" in {

    node
      .upsertRate(Waves, 0, expectedStatusCode = BadRequest)
      .message shouldBe "Asset rate should be positive"

    node.getRates shouldBe defaultRateMap

    node
      .upsertRate(Waves, -0.1, expectedStatusCode = BadRequest)
      .message shouldBe "Asset rate should be positive"

    node.getRates shouldBe defaultRateMap
  }

  "Changing rates affects order validation" in {
    // set rate for btc
    node.upsertRate(btcAsset, 1, expectedStatusCode = Created)

    // place order with admissible fee (according to btc rate = 1)
    val placedOrderId1 = node.placeOrder(getOrder).message.id
    node.waitOrderStatus(wctUsdPair, placedOrderId1, "Accepted")

    // slightly increase rate for btc
    node.upsertRate(btcAsset, 1.1, expectedStatusCode = OK)

    // the same order now is rejected
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr")
    )

    // return previous rate for btc
    node.upsertRate(btcAsset, 1, expectedStatusCode = OK)

    val placedOrderId2 = node.placeOrder(getOrder).message.id
    node.waitOrderStatus(wctUsdPair, placedOrderId2, "Accepted")

    node.deleteRate(btcAsset)
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add high rate for btc
    node.upsertRate(btcAsset, 1.1, expectedStatusCode = Created)

    // order with low fee should be rejected
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr")
    )

    // restart matcher
    docker.restartNode(node)

    // order with low fee should be rejected again
    node.expectIncorrectOrderPlacement(
      getOrder,
      400,
      "OrderRejected",
      Some(s"Required 0.0033 $btcStr as fee for this order, but given 0.003 $btcStr")
    )
  }
}
