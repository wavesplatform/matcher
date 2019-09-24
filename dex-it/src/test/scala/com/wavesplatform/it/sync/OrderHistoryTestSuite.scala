package com.wavesplatform.it.sync

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

import scala.math.BigDecimal.RoundingMode.CEILING

class OrderHistoryTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] =
    super.nodeConfigs.map(
      ConfigFactory
        .parseString("waves.dex.allowed-order-versions = [1, 2, 3]")
        .withFallback
    )

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).foreach { tx =>
      node.waitForTransaction { node.broadcastRequest(tx.json.value).id }
    }
  }

  "Order history should save fee info" in {
    val feeAsset = eth
    node.upsertRate(feeAsset, 0.005, expectedStatusCode = StatusCodes.Created)

    withClue("in placed and cancelled order") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee, version = 3).message.id
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe None
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
              node.ordersByAddress(alice, activeOnly),
              node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe 0
            orderbook.feeAsset shouldBe Waves
          }
        )
      }
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 0
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
            node.ordersByAddress(alice, activeOnly = true),
            node.activeOrderHistory(alice)).foreach(orderbookHistory => orderbookHistory.find(_.id == aliceOrderId) shouldBe None)
    }

    withClue("in placed and cancelled orderV3") {
      val aliceOrderV3Id = node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe None
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
              node.ordersByAddress(alice, activeOnly),
              node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe 0
            orderbook.feeAsset shouldBe feeAsset
          }
        )
      }
      node.cancelOrder(alice, wctUsdPair, aliceOrderV3Id)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 0
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
            node.ordersByAddress(alice, activeOnly = true),
            node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in filled orders of different versions") {
      val aliceOrderV3Id = node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      val bobOrderId     = node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderV3Id)
      Array(bobOrderId, aliceOrderV3Id).foreach((id: String) => node.orderStatus(id, wctUsdPair).filledFee shouldBe Some(matcherFee))
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(bob, wctUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
            node.ordersByAddress(alice, activeOnly = true),
            node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in partially filled and cancelled orders") {
      val aliceOrderV3Id = node.placeOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee).message.id
      val bobOrderId     = node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
              node.ordersByAddress(alice, activeOnly),
              node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe matcherFee / 2
            orderbook.feeAsset shouldBe Waves
          }
        )
      }
      Array(node.orderHistoryByPair(bob, wctUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee / 2
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
            node.ordersByAddress(alice, activeOnly = true),
            node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in partially filled and cancelled orders of different versions") {
      val aliceOrderV3Id =
        node.placeOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      val bobOrderId = node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
              node.ordersByAddress(alice, activeOnly),
              node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe matcherFee / 2
            orderbook.feeAsset shouldBe feeAsset
          }
        )
      }
      Array(node.orderHistoryByPair(bob, wctUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee / 2
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
            node.ordersByAddress(alice, activeOnly = true),
            node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in partially filled orders with fractional filled amount") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 9.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderId)
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe Some(33333)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 33333
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
    }

    withClue("should should right fee if not enoght amount before order execution and fee rounding") {
      val ethBalance = node.tradableBalance(alice, ethUsdPair)(EthId.toString)
      node.broadcastTransfer(alice,
                             bob.toAddress.stringRepr,
                             ethBalance - (BigDecimal(0.005) * matcherFee).toLong,
                             minFee,
                             Some(EthId.toString),
                             None,
                             waitForTx = true)

      node.upsertRate(feeAsset, 0.33333333, expectedStatusCode = StatusCodes.OK)
      val orderFee: Long = (BigDecimal(0.33333333) * matcherFee).setScale(0, CEILING).toLong

      val aliceBuyOrderId =
        node
          .placeOrder(alice, ethUsdPair, BUY, 1.eth, 0.5.price, orderFee, version = 3, feeAsset = feeAsset)
          .message
          .id

      Array(node.orderHistoryByPair(alice, ethUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceBuyOrderId).get
          orderbook.fee shouldBe orderFee
          orderbook.filledFee shouldBe 0
          orderbook.feeAsset shouldBe feeAsset
        }
      )

      node.tradableBalance(bob, ethUsdPair)(EthId.toString)
      val bobOrderId = node.placeOrder(bob, ethUsdPair, SELL, 1.eth, 0.5.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      node.waitOrderInBlockchain(aliceBuyOrderId)
      node.orderStatus(aliceBuyOrderId, ethUsdPair).filledFee shouldBe Some(orderFee)
      Array(node.orderHistoryByPair(alice, ethUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceBuyOrderId).get
          orderbook.fee shouldBe orderFee
          orderbook.filledFee shouldBe orderFee
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(bob, ethUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe feeAsset
        }
      )
    }
  }

}
