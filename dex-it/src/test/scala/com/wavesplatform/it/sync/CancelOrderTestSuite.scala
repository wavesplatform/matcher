package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.util.FutureOps._
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.api.{MatcherStatusResponse, MatcherStatusResponseWithFee, SyncMatcherHttpApi}
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util.{GlobalTimer, TimerExt}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.assets.exchange.OrderType.SELL
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import com.wavesplatform.transaction.transfer.TransferTransactionV2

import scala.concurrent.Future
import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {

  private val wavesBtcPair = AssetPair(Waves, IssuedAsset(BtcId))

  // micro-block-interval and balance-watching-buffer-interval to reproduce an auto cancel issue
  // snapshots-interval - snapshots should not affect this test
  override protected def nodeConfigs: Seq[Config] = super.nodeConfigs.map {
    ConfigFactory
      .parseString(
        s"""waves {
           |  miner.micro-block-interval = 3s
           |  dex {
           |    snapshots-interval = 100000
           |    balance-watching-buffer-interval = 100ms
           |  }
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

      assertBadRequestAndMessage(node.cancelOrder(bob, wavesUsdPair, orderId), s"The order $orderId is canceled")
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

  "Auto cancel" - {
    // TODO: Uncomment after Node v1.1.6
    "wrong auto cancel when match on all coins" ignore {
      val accounts       = (1 to 30).map(i => KeyPair(s"auto-cancel-$i".getBytes(StandardCharsets.UTF_8)))
      val oneOrderAmount = 10000
      val orderPrice     = 3000000000000L

      val initialTransferIds = accounts
        .map { account =>
          TransferTransactionV2
            .selfSigned(
              assetId = Waves,
              sender = alice,
              recipient = account,
              amount = issueFee,
              timestamp = System.currentTimeMillis(),
              feeAssetId = Waves,
              feeAmount = minFee,
              attachment = Array.emptyByteArray
            )
            .explicitGet()
        }
        .map { tx =>
          node.signedBroadcast(tx.json())
          tx.id()
        }

      initialTransferIds.foreach(id => node.waitForTransaction(id.toString))

      val accountsAndAssets = accounts.zipWithIndex.map {
        case (account, i) =>
          account -> IssueTransactionV2
            .selfSigned(
              chainId = AddressScheme.current.chainId,
              sender = account,
              name = s"WowSoMuchCoin-$i".getBytes(StandardCharsets.UTF_8),
              description = Array.emptyByteArray,
              quantity = oneOrderAmount,
              decimals = 2,
              reissuable = false,
              script = None,
              fee = issueFee,
              timestamp = System.currentTimeMillis()
            )
            .explicitGet()
      }

      accountsAndAssets.foreach { case (_, tx) => node.signedBroadcast(tx.json()) }
      accountsAndAssets.foreach { case (_, tx) => node.waitForTransaction(tx.id().toString) }

      val sells = accountsAndAssets.map {
        case (account, asset) =>
          val assetPair = AssetPair(IssuedAsset(asset.id()), Waves)
          assetPair -> node
            .placeOrder(
              sender = account,
              pair = assetPair,
              orderType = OrderType.SELL,
              amount = oneOrderAmount,
              price = orderPrice,
              fee = matcherFee
            )
            .message
            .id
      }

      sells.foreach(Function.tupled(node.waitOrderStatus(_, _, "Accepted")))

      val buyOrders = for {
        (_, asset) <- accountsAndAssets
        i          <- 1 to 10
      } yield
        node
          .prepareOrder(
            sender = alice,
            pair = AssetPair(IssuedAsset(asset.id()), Waves),
            orderType = OrderType.BUY,
            amount = oneOrderAmount / 10,
            price = orderPrice,
            fee = matcherFee,
            timeToLive = 30.days - i.seconds // to make different orders
          )

      SyncMatcherHttpApi.sync(
        {
          import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

          val asyncNode = async(node)
          Future.traverse(buyOrders.groupBy(_.assetPair).values) { orders =>
            Future.inSeries(orders)(asyncNode.placeOrder(_).flatMap { _ =>
              val wait = ThreadLocalRandom.current().nextInt(100, 1200).millis
              GlobalTimer.instance.sleep(wait)
            })
          }
        },
        5.minutes
      )

      val statuses = sells.map {
        case (assetPair, orderId) =>
          orderId -> node
            .waitFor[MatcherStatusResponseWithFee](s"$orderId status")(
              _.orderStatus(orderId, assetPair, waitForStatus = false),
              r => r.status == "Cancelled" || r.status == "Filled",
              1.second
            )
            .status
      }

      statuses.foreach {
        case (orderId, status) =>
          withClue(s"$orderId: ") {
            status shouldBe "Filled"
          }
      }
    }
  }
}
