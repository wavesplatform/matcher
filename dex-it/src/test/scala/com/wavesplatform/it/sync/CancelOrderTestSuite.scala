package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{MatcherStatusResponse, SyncMatcherHttpApi}
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import com.wavesplatform.transaction.transfer.TransferTransactionV2

import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {
  private val wavesBtcPair = AssetPair(Waves, IssuedAsset(BtcId))

  override protected def nodeConfigs: Seq[Config] = super.nodeConfigs.map { orig =>
    ConfigFactory
      .parseString(
        s"""waves {
           |  miner.micro-block-interval = 3s
           |  dex.balance-watching-buffer-interval = 100ms
           |}""".stripMargin
      )
      .withFallback(orig)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val xs = Seq(IssueUsdTx, IssueBtcTx).map(_.json()).map(node.signedBroadcast(_))
    xs.foreach(tx => node.waitForTransaction(tx.id))
  }

  "Order can be canceled" - {
    "After cancelAllOrders (200) all of them should be cancelled" in {
      val orders = new ListBuffer[String]()
      val time   = System.currentTimeMillis

      for (i <- 1 to 200) {
        val order = node
          .placeOrder(node.prepareOrder(bob, wavesBtcPair, OrderType.SELL, 1000000, 123450000L, 300000, version = 2: Byte, creationTime = time + i))
          .message
          .id
        node.waitOrderStatus(wavesUsdPair, order, "Accepted")
        orders += order
      }

      node.cancelAllOrders(bob)

      orders.foreach(order => {
        node.waitOrderStatus(wavesBtcPair, order, "Cancelled")
      })
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
            inSeries(orders)(asyncNode.placeOrder(_).flatMap { _ =>
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
            .waitFor[MatcherStatusResponse](s"$orderId status")(
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

  private def inSeries[A, B](xs: Seq[A])(f: A => Future[B]): Future[Seq[B]] =
    xs.foldLeft(Future.successful(Queue.empty[B])) {
      case (r, curr) =>
        for {
          xs <- r
          x  <- f(curr)
        } yield xs.enqueue(x)
    }
}
