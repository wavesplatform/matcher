package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.time.GlobalTimer
import com.wavesplatform.dex.it.time.TimerOps.TimerOpsImplicits
import com.wavesplatform.dex.util.FutureOps.Implicits
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class CancelOrderTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "Order can be canceled" - {

    "After cancelAllOrders (200) all of them should be cancelled" in {
      val totalAccounts    = 20
      val ordersPerAccount = 200

      val accounts = (1 to totalAccounts).map(i => KeyPair(ByteStr(s"account-test-$i".getBytes(StandardCharsets.UTF_8)))).toList
      broadcastAndAwait(mkMassTransfer(alice, Waves, accounts.map(account => ParsedTransfer(account, 1000.waves))))

      def place(account: KeyPair, startPrice: Long, numOrders: Int): Future[Unit] = {
        val futures = (1 to numOrders).map { i =>
          dex1.asyncApi.place(mkOrder(account, wavesUsdPair, OrderType.SELL, 1.waves, startPrice + i)) // version 2
        }

        Future.sequence(futures).map(_ => ())
      }

      Await.ready(
        for {
          _ <- {
            val pairs = accounts.zipWithIndex.map { case (account, i) => (account, (i + 1) * 1000) }
            Future.inSeries(pairs)(Function.tupled(place(_, _, ordersPerAccount)))
          }
          _ <- Future.traverse(accounts) { account =>
            dex1.asyncApi.orderHistoryByPair(account, wavesUsdPair).map { orders =>
              withClue(s"account $account: ") {
                orders.size shouldBe ordersPerAccount
              }
            }
          }
          _ <- Future.traverse(accounts)(dex1.asyncApi.cancelAll(_))
        } yield (),
        5.minutes
      )

      eventually {
        val orderBook = dex1.api.orderBook(wavesUsdPair)
        orderBook.bids should be(empty)
        orderBook.asks should be(empty)
      }
    }

    "by sender" in {
      val order = mkBobOrder
      placeAndAwait(order)

      dex1.api.cancel(bob, order)
      dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)

      dex1.api.orderHistoryByPair(bob, wavesUsdPair).collectFirst {
        case o if o.id == order.id() => o.status shouldEqual OrderStatus.Cancelled
      }
    }

    "with API key" in {
      val order = mkBobOrder
      placeAndAwait(order)

      dex1.api.cancelWithApiKey(order)
      dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)

      dex1.api.orderHistory(bob).find(_.id == order.id()).get.status shouldBe OrderStatus.Cancelled
      dex1.api.orderHistoryByPair(bob, wavesUsdPair).find(_.id == order.id()).get.status shouldBe OrderStatus.Cancelled

      val orderBook = dex1.api.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when order already cancelled" in {
      val order = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800)
      placeAndAwait(order)
      cancelAndAwait(bob, order)
      dex1.api.tryCancel(bob, order) should failWith(9437194) // OrderCanceled
    }

    "when request sender is not the sender of and order" in {
      val order = mkBobOrder
      placeAndAwait(order)

      val r = dex1.api.tryCancel(matcher, order)
      r shouldBe 'left
      r.left.get.error shouldBe 9437193 // OrderNotFound

      // Cleanup
      dex1.api.cancel(bob, order)
      dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)
    }
  }

  "Batch cancel works for" - {
    "all orders placed by an address" in {
      val orders = mkBobOrders(wavesUsdPair) ::: mkBobOrders(wavesBtcPair)
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))

      dex1.api.cancelAll(bob)

      orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Cancelled))
    }

    "a pair" in {
      val wavesUsdOrders = mkBobOrders(wavesUsdPair)
      val wavesBtcOrders = mkBobOrders(wavesBtcPair)
      val orders         = wavesUsdOrders ::: wavesBtcOrders
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))

      dex1.api.cancelAllByPair(bob, wavesBtcPair)

      wavesBtcOrders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Cancelled))
      wavesUsdOrders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))
    }
  }

  "Auto cancel" - {
    "wrong auto cancel when match on all coins" in {
      val accounts       = (1 to 30).map(i => KeyPair(s"auto-cancel-$i".getBytes(StandardCharsets.UTF_8)))
      val oneOrderAmount = 10000
      val orderPrice     = 3000000000000L

      broadcastAndAwait(mkMassTransfer(alice, Waves, accounts.map(ParsedTransfer(_, issueFee)).toList))

      val accountsAndAssets = accounts.zipWithIndex.map {
        case (account, i) => account -> mkIssue(account, s"WowSoMuchCoin-$i", quantity = oneOrderAmount, decimals = 2)
      }.toMap
      broadcastAndAwait(accountsAndAssets.values.toSeq: _*)

      val sells = accountsAndAssets.map {
        case (account, asset) =>
          val assetPair = AssetPair(IssuedAsset(asset.id()), Waves)
          mkOrder(account, assetPair, OrderType.SELL, oneOrderAmount, orderPrice)
      }

      sells.foreach(dex1.api.place)
      sells.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))

      val buyOrders = for {
        (_, asset) <- accountsAndAssets
        i          <- 1 to 10
      } yield
        mkOrder(alice,
                AssetPair(IssuedAsset(asset.id()), Waves),
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
        order -> dex1.api.waitForOrder(order)(r => r.status == OrderStatus.Cancelled || r.status == OrderStatus.Filled).status
      }

      statuses.foreach {
        case (order, status) =>
          withClue(s"${order.id()}: ") {
            status shouldBe OrderStatus.Filled
          }
      }
    }
  }

  private def mkBobOrder                        = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800)
  private def mkBobOrders(assetPair: AssetPair) = (1 to 5).map(i => mkOrder(bob, assetPair, OrderType.SELL, 100.waves + i, 400)).toList
}
