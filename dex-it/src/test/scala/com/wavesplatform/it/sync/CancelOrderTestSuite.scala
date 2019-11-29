package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.util.FutureOps._
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.SELL
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.Future
import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {

  private val wavesBtcPair = AssetPair(Waves, IssuedAsset(BtcId))

  override protected def nodeConfigs: Seq[Config] =
    super.nodeConfigs.map {
      ConfigFactory
        .parseString(
          s"""waves.dex {
             |  snapshots-interval = 100000
             |}""".stripMargin
        )
        .withFallback
    }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val xs = Seq(IssueUsdTx, IssueBtcTx).map(_.json()).map(node.signedBroadcast(_))
    xs.foreach(tx => node.waitForTransaction(tx.id))
  }

  def createAccountWithBalance(balances: (Long, Option[String])*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${ThreadLocalRandom.current().nextLong()}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) =>
        if (asset.isDefined)
          assert(
            node.assetBalance(alice.toAddress.toString, asset.get.toString).balance >= balance,
            s"Alice doesn't have enough balance in ${asset.get.toString} to make a transfer"
          )
        node.waitForTransaction(node.broadcastTransfer(alice, account.toAddress.toString, balance, 300000L, asset, None).id)
    }
    account
  }

  "Order can be canceled" - {

    "After cancelAllOrders all of them should be cancelled" in {
      val accounts = (1 to 20).map(_ => createAccountWithBalance(100000000000L -> None))

      SyncMatcherHttpApi.sync(
        {
          import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

          val asyncNode        = async(node)
          val ordersPerAccount = 200

          def place(account: KeyPair, startPrice: Long, numOrders: Int): Future[Unit] = {
            val time = System.currentTimeMillis

            val futures = (1 to numOrders).map { c =>
              asyncNode.placeOrder(account, wavesUsdPair, SELL, 100000000L, startPrice + c, 300000L, 2.toByte, timestamp = time + c)
            }

            Future.sequence(futures).map(_ => ())
          }

          def cancelAll(account: KeyPair): Future[Unit] = asyncNode.cancelAllOrders(account, System.currentTimeMillis).map(_ => ())

          for {
            _ <- {
              val pairs = accounts.zipWithIndex.map { case (account, i) => (account, (i + 1) * 1000) }
              Future.inSeries(pairs)(Function.tupled(place(_, _, ordersPerAccount)))
            }
            _ <- Future.traverse(accounts) { account =>
              asyncNode.orderHistoryByPair(account, wavesUsdPair).map { orders =>
                withClue(s"account $account: ") {
                  orders.size shouldBe ordersPerAccount
                }
              }
            }
            _ <- Future.traverse(accounts)(cancelAll)
          } yield ()
        },
        5.minutes
      )

      eventually {
        val orderBook = node.orderBook(wavesUsdPair)
        orderBook.bids should be(empty)
        orderBook.asks should be(empty)
      }
    }

    "by sender" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrder(bob, wavesUsdPair, orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

      node.orderHistoryByPair(bob, wavesUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrderWithApiKey(orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled", 1.minute)

      node.fullOrderHistory(bob).filter(_.id == orderId).head.status shouldBe "Cancelled"
      node.orderHistoryByPair(bob, wavesUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when order already cancelled" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrder(bob, wavesUsdPair, orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled")

      assertBadRequestAndMessage(node.cancelOrder(bob, wavesUsdPair, orderId), s"The order ${orderId} is canceled")
    }

    "when request sender is not the sender of and order" in {
      val orderId = node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800, matcherFee).message.id
      node.waitOrderStatus(wavesUsdPair, orderId, "Accepted", 1.minute)

      node.expectCancelRejected(matcher, wavesUsdPair, orderId)

      // Cleanup
      node.cancelOrder(bob, wavesUsdPair, orderId)
      node.waitOrderStatus(wavesUsdPair, orderId, "Cancelled")
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        node.fullOrderHistory(bob)

        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves + i, 400, matcherFee).message.id
        }

        node.assetBalance(bob.toAddress.stringRepr, BtcId.toString)

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesBtcPair, OrderType.BUY, 100.waves + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Accepted"))

        node.cancelAllOrders(bob)

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Cancelled"))
      }

      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves + i, 400, matcherFee).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, wavesBtcPair, OrderType.BUY, 100.waves + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Accepted"))

        node.cancelOrdersForPair(bob, wavesBtcPair)

        btcOrderIds.foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => node.waitOrderStatus(wavesUsdPair, id, "Accepted"))
      }
    }
  }
}
