package com.wavesplatform.it.sync

import cats.syntax.option._
import sttp.client3._
import sttp.model._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.{HttpAssetInfo, HttpOrderBookHistoryItem, HttpV0LevelAgg, HttpV0OrderBook}
import com.wavesplatform.dex.api.http.headers.MatcherHttpServer
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType._
import com.wavesplatform.dex.domain.order.{AcceptedOrderType, Order, OrderType}
import com.wavesplatform.dex.error.{AssetNotFound, BalanceNotEnough, OrderNotFound, PriceLastDecimalsMustBeZero, UserPublicKeyIsNotValid}
import com.wavesplatform.dex.it.api.responses.dex._
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.config.DexTestConfig.issueAssetPair
import com.wavesplatform.it.tags.SmokeTests
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._

@SmokeTests
class MatcherTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {

  private val aliceSellAmount = 500
  private val exTxFee = matcherFee

  private val aliceAssetName = "Alice-X"
  private val IssueResults(issueAliceAssetTx, _, aliceAsset) = mkIssueExtended(alice, aliceAssetName, 1000, 0)
  private val aliceWavesPair = AssetPair(aliceAsset, Waves)

  private val IssueResults(issueBob1Asset1Tx, _, bobAsset1) = mkIssueExtended(bob, "Bob-1-X", someAssetAmount, 5)
  private val bob1WavesPair = AssetPair(bobAsset1, Waves)

  private val order1 = mkOrder(alice, aliceWavesPair, SELL, aliceSellAmount, 2000.waves, ttl = 10.minutes) // TTL?

  private val maxOrders = 99

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdnId", "$BtcId", "$UsdId", "WAVES", $EthId ]
       |  order-db.max-orders = $maxOrders
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(
      issueAliceAssetTx,
      issueBob1Asset1Tx,
      IssueUsdTx,
      IssueBtcTx,
      IssueUsdnTx,
      IssueEthTx
    )
    dex1.start()
  }

  "Swagger page is available" in {
    val addr = dex1.restApiAddress
    tryHttpBackend.send(basicRequest.response(asString).get(uri"http://${addr.getHostName}:${addr.getPort}/api-docs/index.html")) shouldBe Symbol(
      "success"
    )
  }

  "Check cross ordering between Alice and Bob" - {
    "/matcher should respond with the matcher's public key" in {
      dex1.api.publicKey shouldBe matcher.publicKey
    }

    "/matcher should contain a header with HTTP server" in {
      dex1.httpApi.publicKey.header(MatcherHttpServer.name) shouldBe "matcher-1".some
    }

    "sell order could be placed correctly" - {
      "alice places sell order" in {
        dex1.api.place(order1).status shouldBe "OrderAccepted" // TODO

        // Alice checks that the order in order book
        dex1.api.waitForOrderStatus(order1, Status.Accepted)

        // Alice check that order is correct
        val orders = dex1.api.getOrderBook(aliceWavesPair)
        orders.asks.head.amount shouldBe aliceSellAmount
        orders.asks.head.price shouldBe 2000.waves
      }

      "orderType should be limit for a limit order" in {
        def validateHistory(label: String, orders: Seq[HttpOrderBookHistoryItem]): Unit = withClue(s"$label: ") {
          orders should have size 1
          orders.head.orderType shouldBe AcceptedOrderType.Limit
        }

        validateHistory("by pair", dex1.api.getOrderHistoryByAssetPairAndPKWithSig(alice, aliceWavesPair))
        validateHistory("full", dex1.api.getOrderHistoryByPKWithSig(alice))
        validateHistory("admin", dex1.api.getOrderHistoryByPKWithSig(alice, activeOnly = Some(false)))
      }

      "get opened trading markets" in {
        val orderBooks = dex1.api.getOrderBooks
        orderBooks.markets.size shouldBe 1
        val markets = orderBooks.markets.head

        markets.amountAssetName shouldBe aliceAssetName
        markets.amountAssetInfo shouldBe Some(HttpAssetInfo(issueAliceAssetTx.decimals()))

        markets.priceAssetName shouldBe "WAVES"
        markets.priceAssetInfo shouldBe Some(HttpAssetInfo(8))
      }

      "frozen amount should be listed via matcherBalance REST endpoint" in {
        dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(Waves -> matcherFee, aliceAsset -> aliceSellAmount)
        dex1.api.getReservedBalanceWithApiKey(bob) should have size 0
      }

      "frozen amount should be listed via matcherBalance REST endpoint with Api Key" in {
        dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(Waves -> matcherFee, aliceAsset -> aliceSellAmount)
        dex1.api.getReservedBalanceWithApiKey(bob) should have size 0
      }

      "and should be listed by trader's publiс key via REST" in {
        dex1.api.getOrderHistoryByPKWithSig(alice).map(_.id) should contain(order1.id())
      }

      "and should match with buy order" in {
        val bobBalance = wavesNode1.api.balance(bob, Waves)
        val matcherBalance = wavesNode1.api.balance(matcher, Waves)
        val aliceBalance = wavesNode1.api.balance(alice, Waves)

        // Bob places a buy order
        val order2 = mkOrder(bob, aliceWavesPair, BUY, 200, 2.waves * Order.PriceConstant)
        dex1.api.place(order2).status shouldBe "OrderAccepted"

        dex1.api.waitForOrderStatus(order1, Status.PartiallyFilled)
        dex1.api.waitForOrderStatus(order2, Status.Filled)

        dex1.api.getOrderHistoryByAssetPairAndPKWithSig(bob, aliceWavesPair).map(_.id) should contain(order2.id())
        dex1.api.getOrderHistoryByPKWithSig(bob).map(_.id) should contain(order2.id())

        waitForOrderAtNode(order2)
        eventually {
          // Bob checks that asset on his balance
          wavesNode1.api.balance(bob, aliceAsset) shouldBe 200
        }

        // Alice checks that part of her order still in the order book
        val orders = dex1.api.getOrderBook(aliceWavesPair)
        orders.asks.head.amount shouldBe 300
        orders.asks.head.price shouldBe 2000.waves

        // Alice checks that she sold some assets
        wavesNode1.api.balance(alice, aliceAsset) shouldBe 800

        // Bob checks that he spent some Waves
        val updatedBobBalance = wavesNode1.api.balance(bob, Waves)
        updatedBobBalance shouldBe (bobBalance - 2000 * 200 - matcherFee)

        // Alice checks that she received some Waves
        val updatedAliceBalance = wavesNode1.api.balance(alice, Waves)
        updatedAliceBalance shouldBe (aliceBalance + 2000 * 200 - (matcherFee * 200.0 / 500.0).toLong)

        // Matcher checks that it earn fees
        val updatedMatcherBalance = wavesNode1.api.balance(matcher, Waves)
        updatedMatcherBalance shouldBe (matcherBalance + matcherFee + (matcherFee * 200.0 / 500.0).toLong - exTxFee)
      }

      "request activeOnly orders" in {
        val aliceOrders = dex1.api.getOrderHistoryByPKWithSig(alice, activeOnly = Some(true))
        aliceOrders.map(_.id) shouldBe Seq(order1.id())
        val bobOrders = dex1.api.getOrderHistoryByPKWithSig(bob, activeOnly = Some(true))
        bobOrders.map(_.id) should have size 0
      }

      "submitting sell orders should check availability of asset" in {
        // Bob trying to place order on more assets than he has - order rejected
        val badOrder = mkOrder(bob, aliceWavesPair, SELL, 300, 1900.waves)
        dex1.tryApi.place(badOrder) should failWith(BalanceNotEnough.code)

        // Bob places order on available amount of assets - order accepted
        val order3 = mkOrder(bob, aliceWavesPair, SELL, 150, 1900.waves)
        placeAndAwaitAtDex(order3)

        // Bob checks that the order in the order book
        val orders = dex1.api.getOrderBook(aliceWavesPair)
        orders.asks should contain(HttpV0LevelAgg(150, 1900.waves))
      }

      "buy order should match on few price levels" in {
        val matcherBalance = wavesNode1.api.balance(matcher, Waves)
        val aliceBalance = wavesNode1.api.balance(alice, Waves)
        val bobBalance = wavesNode1.api.balance(bob, Waves)

        // Alice places a buy order
        val order4 = mkOrder(alice, aliceWavesPair, BUY, 350, (21.waves / 10.0 * Order.PriceConstant).toLong)
        dex1.api.place(order4).status shouldBe "OrderAccepted"

        // Where were 2 sells that should fulfill placed order
        dex1.api.waitForOrderStatus(order4, Status.Filled)

        // Check balances
        waitForOrderAtNode(order4)
        eventually {
          wavesNode1.api.balance(alice, aliceAsset) shouldBe 950
          wavesNode1.api.balance(bob, aliceAsset) shouldBe 50

          val updatedMatcherBalance = wavesNode1.api.balance(matcher, Waves)
          updatedMatcherBalance should be(
            matcherBalance - 2 * exTxFee + matcherFee + (matcherFee * 150.0 / 350.0).toLong + (matcherFee * 200.0 / 350.0).toLong + (matcherFee * 200.0 / 500.0).toLong
          )
        }

        val updatedBobBalance = wavesNode1.api.balance(bob, Waves)
        updatedBobBalance should be(bobBalance - matcherFee + 150 * 1900)

        val updatedAliceBalance = wavesNode1.api.balance(alice, Waves)
        updatedAliceBalance should be(
          aliceBalance - (matcherFee * 200.0 / 350.0).toLong - (matcherFee * 150.0 / 350.0).toLong - (matcherFee * 200.0 / 500.0).toLong - 1900 * 150
        )
      }

      "order could be canceled and resubmitted again" in {
        // Alice cancels the very first order (100 left)
        dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, order1).status shouldBe "OrderCanceled"

        // Alice checks that the order book is empty
        val orders1 = dex1.api.getOrderBook(aliceWavesPair)
        orders1.asks.size should be(0)
        orders1.bids.size should be(0)

        // Alice places a new sell order on 100
        dex1.api.place(mkOrder(alice, aliceWavesPair, SELL, 100, 2000.waves)).status shouldBe "OrderAccepted"

        // Alice checks that the order is in the order book
        val orders2 = dex1.api.getOrderBook(aliceWavesPair)
        orders2.asks should contain(HttpV0LevelAgg(100, 2000.waves))
      }

      "buy order should execute all open orders and put remaining in order book" in {
        val matcherBalance = wavesNode1.api.balance(matcher, Waves)
        val aliceBalance = wavesNode1.api.balance(alice, Waves)
        val bobBalance = wavesNode1.api.balance(bob, Waves)

        // Bob places buy order on amount bigger then left in sell orders
        val order5 = mkOrder(bob, aliceWavesPair, BUY, 130, 2000.waves)
        placeAndAwaitAtDex(order5, Status.PartiallyFilled)

        // Check that remaining part of the order is in the order book
        val orders = dex1.api.getOrderBook(aliceWavesPair)
        orders.bids should contain(HttpV0LevelAgg(30, 2000.waves))

        // Check balances
        waitForOrderAtNode(order5)
        eventually {
          wavesNode1.api.balance(alice, aliceAsset) shouldBe 850
          wavesNode1.api.balance(bob, aliceAsset) shouldBe 150

          val updatedMatcherBalance = wavesNode1.api.balance(matcher, Waves)
          updatedMatcherBalance should be(matcherBalance - exTxFee + matcherFee + (matcherFee * 100.0 / 130.0).toLong)
        }

        val updatedBobBalance = wavesNode1.api.balance(bob, Waves)
        updatedBobBalance should be(bobBalance - (matcherFee * 100.0 / 130.0).toLong - 100 * 2000)

        val updatedAliceBalance = wavesNode1.api.balance(alice, Waves)
        updatedAliceBalance should be(aliceBalance - matcherFee + 2000 * 100)
      }

      "request order book for blacklisted pair" in {
        dex1.tryApi.getOrderBook(AssetPair(ForbiddenAsset, Waves)) should failWith(
          AssetNotFound.code,
          MatcherError.Params(assetId = Some(ForbiddenAsset.toString))
        )
      }

      // TODO DEX-992 this is not about UTX Pool, because waitForOrderAtNode waits for a transaction to be mined
      "should consider UTX pool when checking the balance" in {
        wavesNode1.api.balance(alice, bobAsset1) shouldBe 0
        wavesNode1.api.balance(matcher, bobAsset1) shouldBe 0
        wavesNode1.api.balance(bob, bobAsset1) shouldBe someAssetAmount

        def mkBobOrder = mkOrder(bob, bob1WavesPair, SELL, someAssetAmount, 0.005.waves)

        val order6 = mkBobOrder
        placeAndAwaitAtDex(order6)

        // Alice wants to buy all Bob's assets for 1 Wave
        val order7 = mkOrder(alice, bob1WavesPair, BUY, someAssetAmount, 0.005.waves)
        placeAndAwaitAtDex(order7, Status.Filled)

        waitForOrderAtNode(order7)
        // Bob tries to do the same operation, but at now he have no assets
        dex1.tryApi.place(mkBobOrder) should failWith(BalanceNotEnough.code)
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
        (ap08._3, 0, 8)
      )

    "issue assets" in broadcastAndAwait(ap28._1, ap28._2, ap34._1, ap34._2, ap08._1, ap08._2)

    forAll(assets) { (pair: AssetPair, amountDecimals: Int, priceDecimals: Int) =>
      s"Not able to place order, amount decimals =  $amountDecimals, price decimals =  $priceDecimals " in {
        val amount = BigDecimal(10).pow(amountDecimals).toLong
        val valid = BigDecimal(10).pow(8 + priceDecimals - amountDecimals).longValue
        val minInvalid = valid + BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue + 1
        val maxInvalid = valid + BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue - 1
        val o1 = mkOrder(alice, pair, SELL, amount, minInvalid)
        val o2 = mkOrder(alice, pair, SELL, amount, maxInvalid)

        dex1.tryApi.place(o1) should failWith(PriceLastDecimalsMustBeZero.code, MatcherError.Params(insignificantDecimals = Some(6)))
        dex1.tryApi.place(o2) should failWith(PriceLastDecimalsMustBeZero.code, MatcherError.Params(insignificantDecimals = Some(6)))
      }
    }

    forAll(assets) { (pair: AssetPair, amountDecimals: Int, priceDecimals: Int) =>
      s"Able to place order, amount decimals =  $amountDecimals, price decimals =  $priceDecimals " in {
        val amount = BigDecimal(10).pow(amountDecimals + 8).toLong //big amount, because low price
        val minNonZeroInvalid = BigDecimal(10).pow(priceDecimals - amountDecimals + 1).longValue
        dex1.api.place(mkOrder(alice, pair, BUY, amount, minNonZeroInvalid)).status shouldBe "OrderAccepted"
      }
    }
  }

  "Order statuses for old orders" in {
    val (amountAssetTx, priceAssetTx, pair) = issueAssetPair(alice, 2, 8)
    broadcastAndAwait(amountAssetTx, priceAssetTx)

    def mkAliceOrder(i: Int, tpe: OrderType) = mkOrder(alice, pair, tpe, 100L + i, Order.PriceConstant)

    val orders = (1 to (maxOrders + 5)).flatMap { i =>
      List(
        mkAliceOrder(i, OrderType.BUY),
        mkAliceOrder(i, OrderType.SELL)
      )
    }

    orders.foreach(dex1.api.place)
    orders.foreach { order =>
      val status = dex1.api.orderStatusByAssetPairAndId(order).status
      withClue(order.idStr())(status should not be Status.NotFound)
    }
  }

  "Debug information was updated" in {
    val currentOffset = dex1.api.getCurrentOffset
    currentOffset should be > 0L

    val oldestSnapshotOffset = dex1.api.getOldestSnapshotOffset
    oldestSnapshotOffset should be <= currentOffset

    val snapshotOffsets = dex1.api.getAllSnapshotOffsets
    snapshotOffsets.foreach { case (assetPair, offset) =>
      withClue(assetPair) {
        offset should be <= currentOffset
      }
    }
  }

  "Matcher should" - {

    "reject proxy requests if X-User-Public-Key doesn't match query param and process them correctly otherwise " - {

      "/matcher/balance/reserved/{publicKey}" in {
        dex1.tryApi.getReservedBalanceWithApiKey(alice, Some(bob.publicKey)) should failWith(
          UserPublicKeyIsNotValid.code,
          "Provided public key is not correct, reason: invalid public key"
        )
        dex1.tryApi.getReservedBalanceWithApiKey(alice, Some(alice.publicKey)) shouldBe Symbol("right")
      }

      "/matcher/orders/{address}/cancel" in {

        val now = System.currentTimeMillis

        val o1 = mkOrderDP(bob, wavesUsdPair, SELL, 1.waves, 3000.0, ts = now)
        val o2 = mkOrderDP(bob, wavesUsdPair, SELL, 1.waves, 3000.0, ts = now + 100)

        val orderIds = Seq(o1.id(), o2.id())

        Seq(o1, o2).foreach(dex1.api.place)

        dex1.tryApi.cancelOrdersByIdsWithKeyOrSignature(bob, orderIds, Some(alice.publicKey)) should failWith(
          UserPublicKeyIsNotValid.code,
          "Provided public key is not correct, reason: invalid public key"
        )
        dex1.tryApi.cancelOrdersByIdsWithKeyOrSignature(bob, orderIds, Some(bob.publicKey)) shouldBe Symbol("right")
      }

      "/matcher/orders/{address}" in {
        dex1.tryApi.orderHistoryByAddressWithKey(
          owner = bob,
          xUserPublicKey = Some(alice.publicKey)
        ) should failWith(UserPublicKeyIsNotValid.code, "Provided public key is not correct, reason: invalid public key")

        dex1.tryApi.orderHistoryByAddressWithKey(
          owner = bob,
          xUserPublicKey = Some(bob.publicKey)
        ) shouldBe Symbol("right")
      }

      "/matcher/orders/{address}/{orderId}" in {

        val ts = System.currentTimeMillis()

        val order = mkOrderDP(bob, wavesUsdPair, SELL, 1.waves, 4000.0, ts = ts)
        val notPlacedOrder = mkOrderDP(bob, wavesUsdPair, BUY, 2.waves, 0.004)
        val notFoundError = OrderNotFound(notPlacedOrder.id())

        placeAndAwaitAtDex(order)

        dex1.tryApi.getOrderStatusByAddressAndIdWithKey(
          owner = bob,
          orderId = order.id(),
          xUserPublicKey = Some(alice.publicKey)
        ) should failWith(UserPublicKeyIsNotValid.code, "Provided public key is not correct, reason: invalid public key")

        dex1.tryApi.getOrderStatusByAddressAndIdWithKey(
          owner = bob,
          orderId = notPlacedOrder.id(),
          xUserPublicKey = Some(bob.publicKey)
        ) should failWith(notFoundError.code, notFoundError.message.text)

        dex1.tryApi.getOrderStatusByAddressAndIdWithKey(
          owner = bob,
          orderId = order.id(),
          xUserPublicKey = Some(bob.publicKey)
        ) shouldBe Right(
          HttpOrderBookHistoryItem(
            id = order.id(),
            `type` = SELL,
            orderType = AcceptedOrderType.Limit,
            amount = 1.waves,
            filled = 0,
            price = 4000.usd,
            fee = 0.003.waves,
            filledFee = 0,
            feeAsset = Waves,
            timestamp = ts,
            status = Status.Accepted.name,
            assetPair = order.assetPair,
            avgWeighedPrice = 0,
            version = order.version,
            totalExecutedPriceAssets = 0
          )
        )

        dex1.api.cancelAllOrdersWithSig(bob)
      }
    }

    "correctly handle order status info requests (by signature)" in {

      val ts = System.currentTimeMillis
      val order = mkOrderDP(alice, wavesBtcPair, SELL, 1.waves, 500, ts = ts)

      val notPlacedOrder = mkOrderDP(alice, wavesBtcPair, BUY, 1.waves, 500)
      val notFoundError = OrderNotFound(notPlacedOrder.id())

      placeAndAwaitAtDex(order)

      dex1.tryApi.getOrderStatusByPKAndIdWithSig(alice, order) shouldBe Right(
        HttpOrderBookHistoryItem(
          id = order.id(),
          `type` = SELL,
          orderType = AcceptedOrderType.Limit,
          amount = 1.waves,
          filled = 0,
          price = 500.btc,
          fee = 0.003.waves,
          filledFee = 0,
          feeAsset = Waves,
          timestamp = ts,
          status = OrderStatus.Accepted.name,
          assetPair = order.assetPair,
          avgWeighedPrice = 0,
          version = order.version,
          totalExecutedPriceAssets = 0
        )
      )

      dex1.tryApi.getOrderStatusByPKAndIdWithSig(alice, notPlacedOrder) should failWith(notFoundError.code, notFoundError.message.text)
    }

    "correctly calculate average weighed price when submitted becomes counter" in {

      val sellOrder = mkOrder(bob, btcUsdnPair, SELL, 30000000L, 9350000000L)
      val buyOrder1 = mkOrder(alice, btcUsdnPair, BUY, 26779477L, 9351930000L)
      val buyOrder2 = mkOrder(alice, btcUsdnPair, BUY, 25915611L, 9351330000L)

      placeAndAwaitAtDex(buyOrder1)
      placeAndAwaitAtDex(sellOrder, Status.PartiallyFilled)
      placeAndAwaitAtNode(buyOrder2)

      dex1.tryApi.getOrderStatusByPKAndIdWithSig(bob, sellOrder).map(_.avgWeighedPrice) shouldBe Right(9351722813L)

      Seq(alice, bob).foreach { owner =>
        dex1.api.cancelAllOrdersWithSig(owner)
      }
    }

    "not create OrderExecuted event with executed amount = 0 and the last trade should not have amount = 0" in {

      val btcUsdnPairLastTrade = dex1.api.getOrderBookStatus(btcUsdnPair).lastTrade
      val carol = mkAccountWithBalance(26L -> usdn, 1.waves -> Waves)

      val sellOrder = mkOrder(bob, btcUsdnPair, SELL, 345506L, 9337000000L) // 1594779890545
      val buyOrder = mkOrder(carol, btcUsdnPair, BUY, 80902L, 10270700000L) // 1594780069600

      placeAndAwaitAtDex(sellOrder)

      dex1.api.placeMarket(buyOrder)
      dex1.api.waitForOrderStatus(buyOrder, Status.Filled)

      val ob = dex1.api.getOrderBook(btcUsdnPair)

      ob.asks shouldBe List(HttpV0LevelAgg(345506L, 9337000000L))
      ob.bids should have size 0

      dex1.api.getOrderBookStatus(btcUsdnPair).lastTrade should matchTo(btcUsdnPairLastTrade)

      Seq(bob, carol).foreach { owner =>
        dex1.api.cancelAllOrdersWithSig(owner)
      }
    }

    "correctly calculate total executed price assets when fee in price assets" in {

      val carol = mkAccountWithBalance(10.waves -> Waves, 100.usdn -> usdn)

      dex1.api.upsertAssetRate(usdn, 3.63615)

      // matcherFee = baseFee * 10^(feeAssetDecimals - 8) * rate = 3 * 10^5 * 10^(6 - 8) * 3.63615 = 10908.45 = 10909 USDN cents = 0.010909 USDN

      val sellOrder = mkOrderDP(carol, wavesUsdnPair, SELL, 5.waves, 2.44, feeAsset = usdn, matcherFee = 0.010909.usdn)
      val buyOrder = mkOrderDP(alice, wavesUsdnPair, BUY, 5.waves, 2.44)

      placeAndAwaitAtDex(sellOrder)
      placeAndAwaitAtNode(buyOrder)

      val sellOrderStatus = dex1.api.getOrderStatusByPKAndIdWithSig(carol, sellOrder)
      val buyOrderStatus = dex1.api.getOrderStatusByPKAndIdWithSig(alice, buyOrder)

      Seq(sellOrderStatus, buyOrderStatus).foreach(_.totalExecutedPriceAssets shouldBe 12.2.usdn)
      sellOrderStatus.filledFee shouldBe 0.010909.usdn

      dex1.api.deleteAssetRate(usdn)
    }

    "delete order books if their asset pair became invalid" in {

      val sellOrder1 = mkOrderDP(alice, ethWavesPair, SELL, 1, 135)
      placeAndAwaitAtDex(sellOrder1)

      val ob = dex1.api.getOrderBook(ethWavesPair)
      ob.asks should have size 1
      ob.bids should have size 0

      // Before: [ "$UsdnId", "$BtcId", "$UsdId", "WAVES", $EthId ]
      dex1.restartWithNewSuiteConfig(
        ConfigFactory
          .parseString(s"""waves.dex.price-assets = [ "$UsdnId", "$BtcId", "$UsdId", $EthId, "WAVES"]""".stripMargin)
          .withFallback(dexInitialSuiteConfig)
      )

      withClue("The old order book is not available") {
        dex1.httpApi.getOrderBook(ethWavesPair).code shouldBe StatusCode.MovedPermanently
      }

      val wavesEthPair = AssetPair(Waves, eth)
      dex1.api.orderStatusByAssetPairAndId(wavesEthPair, sellOrder1.id()).status shouldBe Status.Accepted

      dex1.api.getOrderBook(wavesEthPair) should matchTo(
        HttpV0OrderBook(
          timestamp = 0,
          pair = wavesEthPair,
          bids = List.empty,
          asks = List.empty
        )
      )

      dex1.restartWithNewSuiteConfig(ConfigFactory.parseString(
        s"""waves.dex {
           |  price-assets = [ "$UsdnId", "$BtcId", "$UsdId", "WAVES", $EthId]
           |  blacklisted-assets  = [$EthId]
           |}""".stripMargin
      ))

      dex1.api.deleteOrderBookWithKey(ethWavesPair)
      eventually {
        dex1.api.getOrderHistoryByPKWithSig(alice, activeOnly = Some(false)).find(_.id == sellOrder1.id()).value.status shouldBe "Cancelled"
      }

      dex1.restartWithNewSuiteConfig(ConfigFactory.parseString(
        s"""waves.dex {
           |  price-assets = [ "$UsdnId", "$BtcId", "$UsdId", $EthId, "WAVES" ]
           |  blacklisted-assets  = []
           |}""".stripMargin
      ))

      placeAndAwaitAtDex(mkOrderDP(alice, wavesEthPair, SELL, 100, 1))
    }
  }
}
