package com.wavesplatform.it.sync

import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.FeeConstants._
import com.wavesplatform.it.api.{AssetDecimalsInfo, LevelResponse, MatcherError, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.assets.exchange._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._

class MatcherTestSuite extends NewMatcherSuiteBase with TableDrivenPropertyChecks {

  private val aliceSellAmount = 500
  private val exTxFee         = matcherFee

  private val aliceAssetName    = "Alice-X"
  private val issueAliceAssetTx = mk(alice, aliceAssetName, 1000, 0)
  private val aliceAssetId      = issueAliceAssetTx.id()
  private val aliceAsset        = IssuedAsset(aliceAssetId)
  private val aliceWavesPair    = AssetPair(aliceAsset, Waves)

  private val issueBob1Asset1Tx = mk(bob, "Bob-1-X", someAssetAmount, 5)
  private val bobAsset1Id       = issueBob1Asset1Tx.id()
  private val bobAsset1         = IssuedAsset(bobAsset1Id)
  private val bob1WavesPair     = AssetPair(bobAsset1, Waves)

  private val issueBob2Asset2Tx = mk(bob, "Bob-2-X", someAssetAmount, 0)
  private val bobAsset2Id       = issueBob2Asset2Tx.id()
  private val bobAsset2         = IssuedAsset(bobAsset2Id)
  private val bob2WavesPair     = AssetPair(bobAsset2, Waves)

  private val order1 = mkOrder(alice, aliceWavesPair, SELL, aliceSellAmount, 2000.waves, ttl = 2.minutes) // TTL?

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(issueAliceAssetTx, issueBob1Asset1Tx, issueBob2Asset2Tx)
  }

  "Check cross ordering between Alice and Bob" - {
    "/matcher should respond with the matcher's public key" in {
      dex1Api.publicKey shouldBe matcher.publicKey
    }

    "sell order could be placed correctly" - {
      "alice places sell order" in {
        dex1Api.place(order1).status shouldBe "OrderAccepted" // TODO

        // Alice checks that the order in order book
        dex1Api.waitForOrderStatus(order1, OrderStatus.Accepted)

        // Alice check that order is correct
        val orders = dex1Api.orderBook(aliceWavesPair)
        orders.asks.head.amount shouldBe aliceSellAmount
        orders.asks.head.price shouldBe 2000.waves
      }

      "get opened trading markets" in {
        val orderBooks = dex1Api.allOrderBooks
        orderBooks.markets.size shouldBe 1
        val markets = orderBooks.markets.head

        markets.amountAssetName shouldBe aliceAssetName
        markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(issueAliceAssetTx.decimals))

        markets.priceAssetName shouldBe "WAVES"
        markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(8))
      }

      "frozen amount should be listed via matcherBalance REST endpoint" in {
        dex1Api.reservedBalance(alice) shouldBe Map(aliceAsset -> aliceSellAmount)
        dex1Api.reservedBalance(bob) shouldBe empty
      }

      "and should be listed by trader's publiс key via REST" in {
        dex1Api.orderHistory(alice).map(_.id) should contain(order1.id())
      }

      "and should match with buy order" in {
        val bobBalance     = wavesNode1Api.balance(bob, Waves)
        val matcherBalance = wavesNode1Api.balance(matcher, Waves)
        val aliceBalance   = wavesNode1Api.balance(alice, Waves)

        // Bob places a buy order
        val order2 = mkOrder(bob, aliceWavesPair, BUY, 200, 2.waves * Order.PriceConstant)
        dex1Api.place(order2).status shouldBe "OrderAccepted"

        dex1Api.waitForOrderStatus(order1, OrderStatus.PartiallyFilled)
        dex1Api.waitForOrderStatus(order2, OrderStatus.Filled)

        dex1Api.orderHistoryByPair(bob, aliceWavesPair).map(_.id) should contain(order2.id())
        dex1Api.orderHistory(bob).map(_.id) should contain(order2.id())

        waitForOrderAtNode(order2.id())
        eventually {
          // Bob checks that asset on his balance
          wavesNode1Api.balance(bob, aliceAsset) shouldBe 200
        }

        // Alice checks that part of her order still in the order book
        val orders = dex1Api.orderBook(aliceWavesPair)
        orders.asks.head.amount shouldBe 300
        orders.asks.head.price shouldBe 2000.waves

        // Alice checks that she sold some assets
        wavesNode1Api.balance(alice, aliceAsset) shouldBe 800

        // Bob checks that he spent some Waves
        val updatedBobBalance = wavesNode1Api.balance(bob, Waves)
        updatedBobBalance shouldBe (bobBalance - 2000 * 200 - matcherFee)

        // Alice checks that she received some Waves
        val updatedAliceBalance = wavesNode1Api.balance(alice, Waves)
        updatedAliceBalance shouldBe (aliceBalance + 2000 * 200 - (matcherFee * 200.0 / 500.0).toLong)

        // Matcher checks that it earn fees
        val updatedMatcherBalance = wavesNode1Api.balance(matcher, Waves)
        updatedMatcherBalance shouldBe (matcherBalance + matcherFee + (matcherFee * 200.0 / 500.0).toLong - exTxFee)
      }

      "request activeOnly orders" in {
        val aliceOrders = dex1Api.orderHistory(alice, activeOnly = Some(true))
        aliceOrders.map(_.id) shouldBe Seq(order1.id())
        val bobOrders = dex1Api.orderHistory(bob, activeOnly = Some(true))
        bobOrders.map(_.id) shouldBe empty
      }

      "submitting sell orders should check availability of asset" in {
        // Bob trying to place order on more assets than he has - order rejected
        val badOrder = mkOrder(bob, aliceWavesPair, SELL, 300, 1900.waves)
        dex1Api.tryPlace(badOrder) should failWith(3147270) // BalanceNotEnough

        // Bob places order on available amount of assets - order accepted
        val order3 = mkOrder(bob, aliceWavesPair, SELL, 150, 1900.waves)
        dex1Api.place(order3)
        dex1Api.waitForOrderStatus(order3, OrderStatus.Accepted)

        // Bob checks that the order in the order book
        val orders = dex1Api.orderBook(aliceWavesPair)
        orders.asks should contain(LevelResponse(150, 1900.waves))
      }

      "buy order should match on few price levels" in {
        val matcherBalance = wavesNode1Api.balance(matcher, Waves)
        val aliceBalance   = wavesNode1Api.balance(alice, Waves)
        val bobBalance     = wavesNode1Api.balance(bob, Waves)

        // Alice places a buy order
        val order4 = mkOrder(alice, aliceWavesPair, BUY, 350, (21.waves / 10.0 * Order.PriceConstant).toLong)
        dex1Api.place(order4).status shouldBe "OrderAccepted"

        // Where were 2 sells that should fulfill placed order
        dex1Api.waitForOrderStatus(order4, OrderStatus.Filled)

        // Check balances
        waitForOrderAtNode(order4.id())
        eventually {
          wavesNode1Api.balance(alice, aliceAsset) shouldBe 950
          wavesNode1Api.balance(bob, aliceAsset) shouldBe 50

          val updatedMatcherBalance = wavesNode1Api.balance(matcher, Waves)
          updatedMatcherBalance should be(
            matcherBalance - 2 * exTxFee + matcherFee + (matcherFee * 150.0 / 350.0).toLong + (matcherFee * 200.0 / 350.0).toLong + (matcherFee * 200.0 / 500.0).toLong)
        }

        val updatedBobBalance = wavesNode1Api.balance(bob, Waves)
        updatedBobBalance should be(bobBalance - matcherFee + 150 * 1900)

        val updatedAliceBalance = wavesNode1Api.balance(alice, Waves)
        updatedAliceBalance should be(
          aliceBalance - (matcherFee * 200.0 / 350.0).toLong - (matcherFee * 150.0 / 350.0).toLong - (matcherFee * 200.0 / 500.0).toLong - 1900 * 150)
      }

      "order could be canceled and resubmitted again" in {
        // Alice cancels the very first order (100 left)
        dex1Api.cancel(alice, order1).status shouldBe "OrderCanceled"

        // Alice checks that the order book is empty
        val orders1 = dex1Api.orderBook(aliceWavesPair)
        orders1.asks.size should be(0)
        orders1.bids.size should be(0)

        // Alice places a new sell order on 100
        dex1Api.place(mkOrder(alice, aliceWavesPair, SELL, 100, 2000.waves)).status shouldBe "OrderAccepted"

        // Alice checks that the order is in the order book
        val orders2 = dex1Api.orderBook(aliceWavesPair)
        orders2.asks should contain(LevelResponse(100, 2000.waves))
      }

      "buy order should execute all open orders and put remaining in order book" in {
        val matcherBalance = wavesNode1Api.balance(matcher, Waves)
        val aliceBalance   = wavesNode1Api.balance(alice, Waves)
        val bobBalance     = wavesNode1Api.balance(bob, Waves)

        // Bob places buy order on amount bigger then left in sell orders
        val order5 = mkOrder(bob, aliceWavesPair, BUY, 130, 2000.waves)
        dex1Api.place(order5)

        // Check that the order is partially filled
        dex1Api.waitForOrderStatus(order5, OrderStatus.PartiallyFilled)

        // Check that remaining part of the order is in the order book
        val orders = dex1Api.orderBook(aliceWavesPair)
        orders.bids should contain(LevelResponse(30, 2000.waves))

        // Check balances
        waitForOrderAtNode(order5.id())
        eventually {
          wavesNode1Api.balance(alice, aliceAsset) shouldBe 850
          wavesNode1Api.balance(bob, aliceAsset) shouldBe 150

          val updatedMatcherBalance = wavesNode1Api.balance(matcher, Waves)
          updatedMatcherBalance should be(matcherBalance - exTxFee + matcherFee + (matcherFee * 100.0 / 130.0).toLong)
        }

        val updatedBobBalance = wavesNode1Api.balance(bob, Waves)
        updatedBobBalance should be(bobBalance - (matcherFee * 100.0 / 130.0).toLong - 100 * 2000)

        val updatedAliceBalance = wavesNode1Api.balance(alice, Waves)
        updatedAliceBalance should be(aliceBalance - matcherFee + 2000 * 100)
      }

      "request order book for blacklisted pair" in {
        dex1Api.tryOrderBook(AssetPair(ForbiddenAsset, Waves)) should failWith(
          11534345,
          MatcherError.Params(assetId = Some(AssetPair.assetIdStr(ForbiddenAsset)))) // AssetNotFound
      }

      "should consider UTX pool when checking the balance" in {
        wavesNode1Api.balance(alice, bobAsset1) shouldBe 0
        wavesNode1Api.balance(matcher, bobAsset1) shouldBe 0
        wavesNode1Api.balance(bob, bobAsset1) shouldBe someAssetAmount

        def mkBobOrder = mkOrder(bob, bob1WavesPair, SELL, someAssetAmount, 0.005.waves)

        val order6 = mkBobOrder
        dex1Api.place(order6)
        dex1Api.waitForOrderStatus(order6, OrderStatus.Accepted)

        // Alice wants to buy all Bob's assets for 1 Wave
        val order7 = mkOrder(alice, bob1WavesPair, BUY, someAssetAmount, 0.005.waves)
        dex1Api.place(order7)
        dex1Api.waitForOrderStatus(order7, OrderStatus.Filled)

        waitForOrderAtNode(order7.id())
        // Bob tries to do the same operation, but at now he have no assets
        dex1Api.tryPlace(mkBobOrder) should failWith(3147270)
      }

      "trader can buy waves for assets with order without having waves" in {
        val bobWavesBalance = wavesNode1Api.balance(bob, Waves)
        wavesNode1Api.balance(alice, bobAsset2) shouldBe 0
        wavesNode1Api.balance(matcher, bobAsset2) shouldBe 0
        wavesNode1Api.balance(bob, bobAsset2) shouldBe someAssetAmount

        // Bob wants to sell all own assets for 1 Wave
        val order8 = mkOrder(bob, bob2WavesPair, SELL, someAssetAmount, 1.waves)
        dex1Api.place(order8)
        dex1Api.waitForOrderStatus(order8, OrderStatus.Accepted)

        // Bob moves all waves to Alice
        val transferAmount = bobWavesBalance - minFee
        broadcastAndAwait(mkTransfer(bob, alice, transferAmount, Waves))

        wavesNode1Api.balance(bob, Waves) shouldBe 0

        // Order should stay accepted
        dex1Api.waitForOrderStatus(order8, OrderStatus.Accepted)

        // Cleanup
        dex1Api.cancel(bob, order8).status shouldBe "OrderCanceled"
        wavesNode1Api.broadcast(mkTransfer(alice, bob, transferAmount, Waves))
      }

      "market status" in {
        val ask       = 5.waves
        val askAmount = 5000000

        val bid       = 10.waves
        val bidAmount = 10000000

        dex1Api.place(mkOrder(bob, bob2WavesPair, SELL, askAmount, ask))

        val resp1 = dex1Api.orderBookStatus(bob2WavesPair)
        resp1.lastPrice shouldBe None
        resp1.lastSide shouldBe None
        resp1.bid shouldBe None
        resp1.bidAmount shouldBe None
        resp1.ask shouldBe Some(ask)
        resp1.askAmount shouldBe Some(askAmount)

        dex1Api.place(mkOrder(alice, bob2WavesPair, BUY, bidAmount, bid))

        val resp2 = dex1Api.orderBookStatus(bob2WavesPair)
        resp2.lastPrice shouldBe Some(ask)
        resp2.lastSide shouldBe Some(OrderType.BUY.toString)
        resp2.bid shouldBe Some(bid)
        resp2.bidAmount shouldBe Some(bidAmount - askAmount)
        resp2.ask shouldBe None
        resp2.askAmount shouldBe None
      }
    }
  }

  "Max 8 price decimals allowed to be non zero" - {
    val ap28 = issueAssetPair(alice, 2, 8)
    val ap34 = issueAssetPair(alice, 3, 4)
    val ap08 = issueAssetPair(alice, 0, 8)

    val assets =
      Table(
        ("pair", "amountDecimals", "priceDecimals"),
        (ap28._3, 2, 8),
        (ap34._3, 3, 4),
        (ap08._3, 0, 8),
      )

    "issue assets" in broadcastAndAwait(ap28._1, ap28._2, ap34._1, ap34._2, ap08._1, ap08._2)

    forAll(assets) { (pair: AssetPair, amountDecimals: Int, priceDecimals: Int) =>
      s"Not able to place order, amount decimals =  $amountDecimals, price decimals =  $priceDecimals " in {
        val amount     = BigDecimal(10).pow(amountDecimals).toLong
        val valid      = BigDecimal(10).pow(8 + priceDecimals - amountDecimals).longValue()
        val minInvalid = valid + BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue() + 1
        val maxInvalid = valid + BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue() - 1
        val o1         = mkOrder(alice, pair, SELL, amount, minInvalid)
        val o2         = mkOrder(alice, pair, SELL, amount, maxInvalid)

        dex1Api.tryPlace(o1) should failWith(9441284, MatcherError.Params(insignificantDecimals = Some(6)))
        dex1Api.tryPlace(o2) should failWith(9441284, MatcherError.Params(insignificantDecimals = Some(6)))
      }
    }

    forAll(assets) { (pair: AssetPair, amountDecimals: Int, priceDecimals: Int) =>
      s"Able to place order, amount decimals =  $amountDecimals, price decimals =  $priceDecimals " in {
        val amount            = BigDecimal(10).pow(amountDecimals + 8).toLong //big amount, because low price
        val minNonZeroInvalid = BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue()
        dex1Api.place(mkOrder(alice, pair, BUY, amount, minNonZeroInvalid)).status shouldBe "OrderAccepted"
      }
    }
  }

  "Order statuses for old orders" in {
    val (amountAssetTx, priceAssetTx, pair) = issueAssetPair(alice, 2, 8)
    broadcastAndAwait(amountAssetTx, priceAssetTx)

    def mkAliceOrder(i: Int, tpe: OrderType) = mkOrder(alice, pair, tpe, 100L + i, Order.PriceConstant)

    val orders = (1 to (OrderDB.OldestOrderIndexOffset + 5)).flatMap { i =>
      List(
        mkAliceOrder(i, OrderType.BUY),
        mkAliceOrder(i, OrderType.SELL)
      )
    }

    orders.foreach(dex1Api.place)
    orders.foreach { order =>
      val status = dex1Api.orderStatus(order).status
      withClue(order.idStr())(status should not be OrderStatus.NotFound)
    }
  }

  "Debug information was updated" in {
    val currentOffset = dex1Api.currentOffset
    currentOffset should be > 0L

    val oldestSnapshotOffset = dex1Api.oldestSnapshotOffset
    oldestSnapshotOffset should be <= currentOffset

    val snapshotOffsets = dex1Api.allSnapshotOffsets
    snapshotOffsets.foreach {
      case (assetPair, offset) =>
        withClue(assetPair) {
          offset should be <= currentOffset
        }
    }
  }
}
