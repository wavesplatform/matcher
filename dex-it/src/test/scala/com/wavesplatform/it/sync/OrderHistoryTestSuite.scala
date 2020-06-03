package com.wavesplatform.it.sync

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.AddressActor.OrderListType
import com.wavesplatform.dex.api.ApiOrderBookHistoryItem
import com.wavesplatform.dex.api.ApiOrderStatus.Status
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.model.Normalization
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType._
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.math.BigDecimal.RoundingMode.CEILING

class OrderHistoryTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueEthTx)
    dex1.start()
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

    "in placed and cancelled order" - {
      List[(Byte, Asset)](
        (1, Waves),
        (2, Waves),
        (3, Waves),
        (3, eth)
      ).foreach {
        case (version, feeAsset) =>
          s"version=$version, asset=$feeAsset" in {
            val order   = mkOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee = matcherFee, feeAsset = feeAsset, version = version)
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
      val aliceOrder   = mkOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee = matcherFee, feeAsset = feeAsset)
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

    "in partially filled and cancelled orders of different versions" in {
      val aliceOrder   = mkOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee = matcherFee, feeAsset = feeAsset)
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

    "in partially filled orders with fractional filled amount" in {
      val order   = mkOrder(alice, wctUsdPair, BUY, 9.wct, 1.price, matcherFee = matcherFee, feeAsset = feeAsset)
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

    "should save right fee considering the fee rate" in {
      val rate     = 0.33333333
      val orderFee = (BigDecimal(rate) * matcherFee).setScale(0, CEILING).toLong

      val ethBalance = dex1.api.tradableBalance(alice, ethUsdPair)(eth)

      broadcastAndAwait(mkTransfer(alice, bob, ethBalance - orderFee, eth))

      dex1.api.upsertRate(feeAsset, rate)

      val aliceOrder   = mkOrder(alice, ethUsdPair, BUY, 1.eth, 0.5.price, orderFee, feeAsset = feeAsset)
      val aliceOrderId = aliceOrder.id()
      dex1.api.place(aliceOrder)

      orderHistory(alice, ethUsdPair, activeOnly = Some(false)).foreach { orderBookHistory =>
        val item = orderBookHistory.find(_.id == aliceOrderId).get
        item.fee shouldBe orderFee
        item.filledFee shouldBe 0
        item.feeAsset shouldBe feeAsset
      }

      val bobOrder   = mkOrder(bob, ethUsdPair, SELL, 1.eth, 0.5.price, matcherFee, feeAsset = feeAsset)
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

  "OrderHistory should" - {
    "correctly save average weighed price" in {
      Seq(alice, bob).foreach { dex1.api.cancelAll(_) }

      def assertAvgWeighedPrice(keyPair: KeyPair, avgWeighedPrices: List[Long]): Unit = {
        dex1.api.orderHistoryByPair(keyPair, wavesUsdPair, Some(false)).map(_.avgWeighedPrice) should matchTo(avgWeighedPrices)
      }

      // checking market and limit orders because
      // in case of market sell order avgWeighedPrice retrieved from orderDB,
      // in case of limit sell order - from active orders
      Seq(true, false) foreach { isMarketOrder =>
        val mozart  = mkAccountWithBalance(100.waves -> Waves)
        val salieri = mkAccountWithBalance(300.usd   -> usd, 10.waves -> Waves)

        Seq(
          30.waves -> 3.2,
          10.waves -> 2.9,
          50.waves -> 2.7
        ).foreach { case (amount, price) => placeAndAwaitAtDex(mkOrderDP(salieri, wavesUsdPair, BUY, amount, price)) }

        placeAndAwaitAtNode(mkOrderDP(mozart, wavesUsdPair, SELL, 95.waves, 2.0), isMarketOrder = isMarketOrder)

        assertAvgWeighedPrice(mozart, List(288L))
        assertAvgWeighedPrice(salieri, List(270L, 290L, 320L))

        Seq(alice, bob, mozart, salieri).foreach { dex1.api.cancelAll(_) }
      }
    }

    "return an order history with different filters" in {
      val carol = mkAccountWithBalance(10.waves -> Waves)

      val order1 = mkOrderDP(carol, wavesUsdPair, OrderType.SELL, 1.waves, 2.0)
      dex1.api.place(order1)

      placeAndAwaitAtDex(mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 1.waves, 2.0), Status.Filled)
      dex1.api.waitForOrderStatus(order1, Status.Filled)

      val order2 = mkOrderDP(carol, wavesUsdPair, OrderType.SELL, 2.waves, 3.0)
      dex1.api.place(order2)

      val all        = List(order2.id(), order1.id())
      val activeOnly = List(order2.id())
      val closedOnly = List(order1.id())

      withClue("default: ") {
        // MatcherApiRoute.getAssetPairAndPublicKeyOrderHistory
        dex1.api.orderHistoryByPair(carol, wavesUsdPair).map(_.id) should matchTo(all)

        // MatcherApiRoute.getPublicKeyOrderHistory
        dex1.api.orderHistory(carol).map(_.id) should matchTo(all)

        // MatcherApiRoute.getAllOrderHistory
        dex1.api.orderHistoryWithApiKey(carol).map(_.id) should matchTo(activeOnly)
      }

      List(
        // format: off
        (true.some,   none,         OrderListType.ActiveOnly),
        (false.some,  none,         OrderListType.All),
        (none,        true.some,    OrderListType.ClosedOnly),
        (none,        false.some,   OrderListType.All),
        (true.some,   true.some,    OrderListType.Empty),
        (false.some,  true.some,    OrderListType.ClosedOnly),
        (true.some,   false.some,   OrderListType.ActiveOnly),
        (false.some,  false.some,   OrderListType.All),
        // format: on
      ).foreach {
        case (activeOnlyParam, closedOnlyParam, result) =>
          withClue(s"activeOnly=$activeOnlyParam, closedOnly=$closedOnlyParam, result=$result: ") {
            val expected = result match {
              case OrderListType.All        => all
              case OrderListType.Empty      => List.empty
              case OrderListType.ActiveOnly => activeOnly
              case OrderListType.ClosedOnly => closedOnly
            }

            dex1.api.orderHistoryByPair(carol, wavesUsdPair, activeOnlyParam, closedOnlyParam).map(_.id) should matchTo(expected)
            dex1.api.orderHistory(carol, activeOnlyParam, closedOnlyParam).map(_.id) should matchTo(expected)
            dex1.api.orderHistoryWithApiKey(carol, activeOnlyParam, closedOnlyParam).map(_.id) should matchTo(expected)
          }
      }

      dex1.api.cancelAll(carol)
    }
  }

  private def orderHistory(account: KeyPair, pair: AssetPair, activeOnly: Option[Boolean]): List[List[ApiOrderBookHistoryItem]] = List(
    dex1.api.orderHistory(account, activeOnly),
    dex1.api.orderHistoryByPair(account, pair, activeOnly),
    dex1.api.orderHistoryWithApiKey(account, activeOnly)
  )
}
