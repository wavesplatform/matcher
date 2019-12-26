package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.it.api.responses.dex._
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.assets.exchange._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.math.BigDecimal.RoundingMode.CEILING

class OrderHistoryTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx)
    dex1.api.upsertRate(eth, 0.005)
  }

  implicit class DoubleOps(value: Double) {
    val wct, usd: Long = Normalization.normalizeAmountAndFee(value, 2)
    val eth, btc: Long = Normalization.normalizeAmountAndFee(value, 8)

    val price: Long         = Normalization.normalizePrice(value, 2, 2)
    val wctUsdPrice: Long   = Normalization.normalizePrice(value, 2, 2)
    val wavesUsdPrice: Long = Normalization.normalizePrice(value, 8, 2)
  }

  "Order history should save fee info" - {
    val feeAsset = eth

    "in placed and cancelled order" in {
      List[(Byte, Asset)](
        (1, Waves),
        (2, Waves),
        (3, Waves),
        (3, eth)
      ).foreach {
        case (version, feeAsset) =>
          s"version=$version, asset=$feeAsset" in {
            val order   = mkOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee = matcherFee, matcherFeeAssetId = feeAsset, version = version)
            val orderId = order.id()
            dex1.api.place(order)
            dex1.api.orderStatus(order).filledFee shouldBe None

            for {
              activeOnly       <- List(true, false).map(Option(_))
              orderBookHistory <- orderHistory(alice, wctUsdPair, activeOnly)
            } yield {
              val item = orderBookHistory.find(_.id == orderId).get
              item.fee shouldBe matcherFee
              item.filledFee shouldBe 0
              item.feeAsset shouldBe feeAsset
            }

            dex1.api.cancel(alice, order)

            orderHistory(alice, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
              val item = orderBookHistory.find(_.id == orderId).get
              item.fee shouldBe matcherFee
              item.filledFee shouldBe 0
              item.feeAsset shouldBe feeAsset
            }

            orderHistory(alice, wctUsdPair, activeOnly = Some(true)).foreach {
              _.find(_.id == orderId) shouldBe None
            }
          }
      }
    }

    "in filled orders of different versions" in {
      val aliceOrder   = mkOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee = matcherFee, matcherFeeAssetId = feeAsset)
      val aliceOrderId = aliceOrder.id()
      dex1.api.place(aliceOrder)

      val bobOrder   = mkOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee = matcherFee)
      val bobOrderId = bobOrder.id()
      dex1.api.place(bobOrder)

      waitForOrderAtNode(aliceOrder)

      List(bobOrder, aliceOrder).foreach(order => dex1.api.orderStatus(order).filledFee shouldBe Some(matcherFee))

      orderHistory(alice, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee
        item.feeAsset shouldBe feeAsset
      }

      orderHistory(bob, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == bobOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee
        item.feeAsset shouldBe Waves
      }

      orderHistory(alice, wctUsdPair, activeOnly = Some(true)).foreach {
        _.find(_.id == aliceOrderId) shouldBe None
      }
    }

    "in partially filled and cancelled orders" in {
      val aliceOrder   = mkOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee = matcherFee)
      val aliceOrderId = aliceOrder.id()
      dex1.api.place(aliceOrder)

      val bobOrder   = mkOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee = matcherFee)
      val bobOrderId = bobOrder.id()
      dex1.api.place(bobOrder)

      waitForOrderAtNode(aliceOrder)

      dex1.api.orderStatus(aliceOrder).filledFee shouldBe Some(matcherFee / 2)
      dex1.api.orderStatus(bobOrder).filledFee shouldBe Some(matcherFee)

      for {
        activeOnly       <- List(true, false).map(Option(_))
        orderBookHistory <- orderHistory(alice, wctUsdPair, activeOnly)
      } yield {
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee / 2
        item.feeAsset shouldBe Waves
      }

      orderHistory(bob, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == bobOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee
        item.feeAsset shouldBe Waves
      }

      dex1.api.cancel(alice, aliceOrder)
      dex1.api.orderStatus(aliceOrder).filledFee shouldBe Some(matcherFee / 2)
      dex1.api.orderStatus(bobOrder).filledFee shouldBe Some(matcherFee)

      orderHistory(alice, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee / 2
        item.feeAsset shouldBe Waves
      }

      orderHistory(alice, wctUsdPair, activeOnly = Some(true)).foreach {
        _.find(_.id == aliceOrderId) shouldBe None
      }
    }

    withClue("in partially filled and cancelled orders of different versions") {
      val aliceOrder   = mkOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee = matcherFee, matcherFeeAssetId = feeAsset)
      val aliceOrderId = aliceOrder.id()
      dex1.api.place(aliceOrder)

      val bobOrder   = mkOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee = matcherFee)
      val bobOrderId = bobOrder.id()
      dex1.api.place(bobOrder)

      waitForOrderAtNode(aliceOrder)

      dex1.api.orderStatus(aliceOrder).filledFee shouldBe Some(matcherFee / 2)
      dex1.api.orderStatus(bobOrder).filledFee shouldBe Some(matcherFee)

      for {
        activeOnly       <- List(true, false).map(Option(_))
        orderBookHistory <- orderHistory(alice, wctUsdPair, activeOnly)
      } yield {
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee / 2
        item.feeAsset shouldBe feeAsset
      }

      orderHistory(bob, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == bobOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee
        item.feeAsset shouldBe Waves
      }

      dex1.api.cancel(alice, aliceOrder)
      dex1.api.orderStatus(aliceOrder).filledFee shouldBe Some(matcherFee / 2)
      dex1.api.orderStatus(bobOrder).filledFee shouldBe Some(matcherFee)

      orderHistory(alice, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee / 2
        item.feeAsset shouldBe feeAsset
      }

      orderHistory(alice, wctUsdPair, activeOnly = Some(true)).foreach {
        _.find(_.id == aliceOrderId) shouldBe None
      }
    }

    withClue("in partially filled orders with fractional filled amount") {
      val order   = mkOrder(alice, wctUsdPair, BUY, 9.wct, 1.price, matcherFee = matcherFee, matcherFeeAssetId = feeAsset)
      val orderId = order.id()
      dex1.api.place(order)

      dex1.api.place(mkOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee = matcherFee))

      waitForOrderAtNode(order)
      dex1.api.orderStatus(order).filledFee shouldBe Some(33333)

      orderHistory(alice, wctUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == orderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe 33333
        item.feeAsset shouldBe feeAsset
      }

      dex1.api.cancel(alice, order)
    }

    withClue("should should right fee if not enough amount before order execution and fee rounding") {
      val ethBalance = dex1.api.tradableBalance(alice, ethUsdPair)(eth)

      broadcastAndAwait(mkTransfer(alice, bob, ethBalance - (BigDecimal(0.005) * matcherFee).toLong, eth))

      val rate = 0.33333333
      dex1.api.upsertRate(feeAsset, rate)
      val orderFee = (BigDecimal(rate) * matcherFee).setScale(0, CEILING).toLong

      val aliceOrder   = mkOrder(alice, ethUsdPair, BUY, 1.eth, 0.5.price, orderFee, matcherFeeAssetId = feeAsset)
      val aliceOrderId = aliceOrder.id()
      dex1.api.place(aliceOrder)

      orderHistory(alice, ethUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe orderFee
        item.filledFee shouldBe 0
        item.feeAsset shouldBe feeAsset
      }

      val bobOrder   = mkOrder(bob, ethUsdPair, SELL, 1.eth, 0.5.price, matcherFee, matcherFeeAssetId = feeAsset)
      val bobOrderId = bobOrder.id()
      dex1.api.place(bobOrder)

      waitForOrderAtNode(aliceOrder)
      dex1.api.orderStatus(aliceOrder).filledFee shouldBe Some(orderFee)

      orderHistory(alice, ethUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe orderFee
        item.filledFee shouldBe orderFee
        item.feeAsset shouldBe feeAsset
      }

      orderHistory(bob, ethUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == bobOrderId).get
        item.fee shouldBe matcherFee
        item.filledFee shouldBe matcherFee
        item.feeAsset shouldBe feeAsset
      }
    }
  }

  private def orderHistory(account: KeyPair, pair: AssetPair, activeOnly: Option[Boolean]): List[List[OrderBookHistoryItem]] = List(
    dex1.api.orderHistory(account, activeOnly),
    dex1.api.orderHistoryByPair(account, wctUsdPair, activeOnly),
    dex1.api.orderHistoryWithApiKey(account, activeOnly)
  )
}
