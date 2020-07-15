package com.wavesplatform.it.sync

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.effect.FutureOps.Implicits
import com.wavesplatform.dex.it.time.GlobalTimer
import com.wavesplatform.dex.it.time.GlobalTimer.TimerOpsImplicits
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.wavesj.Transfer

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class CancelOrderTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  private var knownAccounts = List(alice, bob)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    knownAccounts.foreach(dex1.api.cancelAll(_))
    eventually {
      val orderBook = dex1.api.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Order can be canceled" - {
    "by sender" in {
      val order = mkBobOrder
      placeAndAwaitAtDex(order)

      dex1.api.cancel(bob, order)
      dex1.api.waitForOrderStatus(order, Status.Cancelled)

      dex1.api.orderHistoryByPair(bob, wavesUsdPair).collectFirst {
        case o if o.id == order.id() => o.status shouldEqual Status.Cancelled.name
      }

      eventually {
        val orderBook = dex1.api.orderBook(wavesUsdPair)
        orderBook.bids shouldBe empty
        orderBook.asks shouldBe empty
      }
    }

    "array of orders could be cancelled with API key" in {
      val acc = mkAccountWithBalance(100.waves -> Waves)
      knownAccounts = acc :: knownAccounts

      val ids = for { i <- 1 to 10 } yield {
        val o = mkOrder(acc, wavesUsdPair, OrderType.SELL, i.waves, 100 + i)
        placeAndAwaitAtDex(o)
        o.id.value
      }

      dex1.api.cancelAllByIdsWithApiKey(acc, ids.toSet)

      ids.map(dex1.api.waitForOrderStatus(wavesUsdPair, _, Status.Cancelled))

      eventually {
        val orderBook = dex1.api.orderBook(wavesUsdPair)
        orderBook.bids shouldBe empty
        orderBook.asks shouldBe empty
      }
    }

    "only owners orders should be cancelled with API key if one of them from another owner" in {
      val acc = mkAccountWithBalance(100.waves -> Waves)
      knownAccounts = acc :: knownAccounts

      val o = mkOrder(alice, wavesUsdPair, OrderType.SELL, 1.waves, 100)
      placeAndAwaitAtDex(o)

      val ids = for { i <- 1 to 10 } yield {
        val o = mkOrder(acc, wavesUsdPair, OrderType.SELL, i.waves, 100 + i)
        placeAndAwaitAtDex(o)
        o.id()
      }

      val allIds = ids :+ o.id()

      dex1.api.cancelAllByIdsWithApiKey(acc, allIds.toSet)

      dex1.api.waitForOrderStatus(o, Status.Accepted)
      ids.map(dex1.api.waitForOrderStatus(wavesUsdPair, _, Status.Cancelled))

      dex1.api.cancel(alice, o)
    }

    "with API key" - {
      "and without X-User-Public-Key" in {
        val order = mkBobOrder
        placeAndAwaitAtDex(order)

        dex1.api.cancelWithApiKey(order)
        dex1.api.waitForOrderStatus(order, Status.Cancelled)

        dex1.api.orderHistory(bob).find(_.id == order.id()).get.status shouldBe Status.Cancelled.name

        dex1.api.orderHistoryByPair(bob, wavesUsdPair).find(_.id == order.id()).get.status shouldBe Status.Cancelled.name

        eventually {
          val orderBook = dex1.api.orderBook(wavesUsdPair)
          orderBook.bids shouldBe empty
          orderBook.asks shouldBe empty
        }
      }
      "and with a valid X-User-Public-Key" in {
        val order = mkBobOrder
        placeAndAwaitAtDex(order)

        dex1.api.cancelWithApiKey(order, Some(order.senderPublicKey))
        dex1.api.waitForOrderStatus(order, Status.Cancelled)

        dex1.api.orderHistory(bob).find(_.id == order.id()).get.status shouldBe Status.Cancelled.name

        dex1.api.orderHistoryByPair(bob, wavesUsdPair).find(_.id == order.id()).get.status shouldBe Status.Cancelled.name
      }

      "and with an invalid X-User-Public-Key" in {
        val order = mkBobOrder
        placeAndAwaitAtDex(order)

        dex1.api.tryCancelWithApiKey(order.id(), Some(alice.publicKey)) should failWith(9437193) // OrderNotFound
        dex1.api.cancelWithApiKey(order)
        dex1.api.waitForOrderStatus(order, Status.Cancelled)
      }
    }
  }

  "Cancel is rejected" - {
    "when order already cancelled" in {
      val order = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800)
      placeAndAwaitAtDex(order)
      cancelAndAwait(bob, order)
      dex1.api.tryCancel(bob, order) should failWith(9437194) // OrderCanceled
    }

    "when order is fully filled" in {
      val order = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 500)

      placeAndAwaitAtDex(order)
      placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 500))

      dex1.api.tryCancel(bob, order) should failWith(9437191, s"The order ${order.id()} is filled")
    }

    "when request sender is not the sender of and order" in {
      val order = mkBobOrder
      placeAndAwaitAtDex(order)

      val r = dex1.api.tryCancel(matcher, order)
      r shouldBe Symbol("left")
      r.left.get.error shouldBe 9437193 // OrderNotFound

      // Cleanup
      dex1.api.cancel(bob, order)
      dex1.api.waitForOrderStatus(order, Status.Cancelled)
    }
  }

  "Batch cancel works for" - {
    "all orders placed by an address" in {
      val orders = mkBobOrders(wavesUsdPair) ::: mkBobOrders(wavesBtcPair)
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Accepted))

      dex1.api.cancelAll(bob)
      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
    }

    "a pair" in {
      val wavesUsdOrders = mkBobOrders(wavesUsdPair)
      val wavesBtcOrders = mkBobOrders(wavesBtcPair)
      val orders         = wavesUsdOrders ::: wavesBtcOrders
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Accepted))

      dex1.api.cancelAllByPair(bob, wavesBtcPair)

      wavesBtcOrders.foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
      wavesUsdOrders.foreach(dex1.api.waitForOrderStatus(_, Status.Accepted))

      dex1.api.cancelAllByPair(bob, wavesUsdPair)
      wavesUsdOrders.foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
    }
  }

  "Batch cancel by id" - {
    "works for specified orders placed by an address" in {
      val orders = mkBobOrders(wavesUsdPair) ::: mkBobOrders(wavesBtcPair)
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Accepted))

      dex1.api.cancelAllByIdsWithApiKey(bob, orders.map(_.id()).toSet)

      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
    }

    // DEX-548
    "returns a rejected orders if an owner is invalid" ignore {
      val orders = mkBobOrders(wavesUsdPair)
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Accepted))

      dex1.api.cancelAllByIdsWithApiKey(alice, orders.map(_.id()).toSet)
      // here is validation
    }
  }

  "Auto cancel" - {
    "wrong cancel when executing a big order by small amount" in {
      val amount             = 835.85722414.waves
      val traderTotalBalance = amount + matcherFee
      val trader             = createAccountWithBalance(traderTotalBalance -> Waves)

      eventually {
        dex1.api.tradableBalance(trader, wavesUsdPair).getOrElse(Waves, 0L) shouldBe traderTotalBalance
      }

      knownAccounts = trader :: knownAccounts

      // Spending all assets
      val counterOrder   = mkOrderDP(trader, wavesUsdPair, OrderType.SELL, amount, 9032, version = 3)
      val submittedOrder = mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 0.0001.waves, 9097)

      placeAndAwaitAtDex(counterOrder)
      placeAndAwaitAtNode(submittedOrder)

      dex1.api.orderStatus(counterOrder).status shouldBe Status.PartiallyFilled
      dex1.api.orderStatus(submittedOrder).status shouldBe Status.Filled
    }

    "wrong cancel when match on all coins" in {
      val accounts = (1 to 30).map(_ => createAccountWithBalance(issueFee -> Waves))
      knownAccounts = knownAccounts ++ accounts

      val oneOrderAmount = 10000
      val orderPrice     = 3000000000000L

      broadcastAndAwait(mkMassTransfer(alice, Waves, accounts.map(x => new Transfer(x.toAddress, issueFee + matcherFee)).toList))

      val accountsAndAssets = accounts.zipWithIndex.map {
        case (account, i) => account -> mkIssue(account, s"WowSoMuchCoin-$i", quantity = oneOrderAmount, decimals = 2)
      }.toMap
      broadcastAndAwait(accountsAndAssets.values.toSeq: _*)

      val sells = accountsAndAssets.map {
        case (account, asset) =>
          val issuedAsset = IssuedAsset(asset.getId)
          val assetPair   = AssetPair(issuedAsset, Waves)
          eventually {
            dex1.api.tradableBalance(account, assetPair).getOrElse(issuedAsset, 0L) shouldBe oneOrderAmount
          }
          mkOrder(account, assetPair, OrderType.SELL, oneOrderAmount, orderPrice)
      }

      sells.foreach(placeAndAwaitAtDex(_))

      val buyOrders = for {
        (_, asset) <- accountsAndAssets
        i          <- 1 to 10
      } yield
        mkOrder(alice,
                AssetPair(IssuedAsset(asset.getId), Waves),
                OrderType.BUY,
                amount = oneOrderAmount / 10,
                price = orderPrice,
                ttl = 30.days - i.seconds) // to make different orders

      Await.ready(
        {
          Future.traverse(buyOrders.groupBy(_.assetPair).values) { orders =>
            Future.inSeries(orders)(dex1.asyncApi.place(_).flatMap { _ =>
              val wait = ThreadLocalRandom.current().nextInt(100, 1200).millis
              GlobalTimer.instance.sleep(wait)
            })
          }
        },
        5.minutes
      )

      val statuses = sells.map { order =>
        order -> dex1.api.waitForOrder(order)(r => r.status == Status.Cancelled || r.status == Status.Filled).status
      }

      statuses.foreach {
        case (order, status) =>
          withClue(s"${order.id()}: ") {
            status shouldBe Status.Filled
          }
      }
    }
  }

  "After cancelAllOrders (200) all of them should be cancelled" in {
    val totalAccounts    = 20
    val ordersPerAccount = 200

    val accounts = (1 to totalAccounts).map(_ => mkAccountWithBalance(1000.waves -> Waves)).toList
    knownAccounts = knownAccounts ++ accounts

    accounts.foreach { account =>
      eventually {
        dex1.api.tradableBalance(account, wavesUsdPair).getOrElse(Waves, 0L) shouldBe 1000.waves
      }
    }

    def place(account: KeyPair, startPrice: Long, numOrders: Int): Future[Seq[Order.Id]] = {
      val orders = (1 to numOrders).map { i =>
        mkOrder(account, wavesUsdPair, OrderType.SELL, 1.waves, startPrice + i) // version 2
      }

      val futures = orders.map(dex1.asyncApi.place)
      Future.sequence(futures).map(_ => orders.map(_.id()))
    }

    Await.ready(
      for {
        orderIds <- {
          val pairs = accounts.zipWithIndex.map { case (account, i) => (account, (i + 1) * 1000) }
          Future.inSeries(pairs)(Function.tupled(place(_, _, ordersPerAccount))).map(_.flatten)
        }
        _ <- Future.traverse(accounts) { account =>
          dex1.asyncApi.orderHistoryByPair(account, wavesUsdPair).map { orders =>
            withClue(s"account $account: ") {
              orders.size shouldBe ordersPerAccount
            }
          }
        }
        _         <- Future.traverse(accounts)(dex1.asyncApi.cancelAll(_))
        _         <- Future.inSeries(orderIds)(dex1.asyncApi.waitForOrderStatus(wavesUsdPair, _, Status.Cancelled))
        orderBook <- dex1.asyncApi.orderBook(wavesUsdPair)
      } yield {
        orderBook.bids should be(empty)
        orderBook.asks should be(empty)
      },
      5.minutes
    )
  }

  private def mkBobOrder                        = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 100.waves, 8)
  private def mkBobOrders(assetPair: AssetPair) = (1 to 5).map(i => mkOrder(bob, assetPair, OrderType.SELL, 100.waves + i, 400)).toList
}
