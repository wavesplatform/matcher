package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.effect.Implicits.FutureCompanionOps
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItExternalKafkaRequired
import im.mak.waves.transactions.ExchangeTransaction
import im.mak.waves.transactions.mass.Transfer

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

@DexItExternalKafkaRequired
class AutoCancelOrderTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  protected lazy val dex2: DexContainer = createDex("dex-2")

  private var knownAccounts = List(alice, bob)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
    dex2.start()
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    knownAccounts.foreach(dex1.api.cancelAll(_))
    eventually {
      val orderBook = dex1.api.getOrderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Auto cancel" - {
    "wrong cancel when match orders on all coins" in {
      info("initializing: transfer initial balances and creating assets")
      val accounts = (1 to 5).map(createAccountWithBalance(_))
      knownAccounts = knownAccounts ++ accounts

      val oneOrderAmount = 10000
      val orderPrice = 3000000000000L

      broadcastAndAwait(mkMassTransfer(alice, Waves, accounts.map(x => new Transfer(x.toAddress, issueFee + matcherFee)).toList))

      val accountsAndAssets = accounts.zipWithIndex.map {
        case (account, i) => account -> mkIssue(account, s"WowSoMuchCoin-$i", quantity = oneOrderAmount, decimals = 2)
      }.toMap
      broadcastAndAwait(accountsAndAssets.values.toSeq: _*)

      info("placing sell orders")
      val now = System.currentTimeMillis()
      val sells = accountsAndAssets.zipWithIndex.map {
        case ((account, asset), i) =>
          val issuedAsset = IssuedAsset(asset.id())
          val assetPair = AssetPair(issuedAsset, Waves)
          eventually {
            dex1.api.getTradableBalance(account, assetPair) should matchTo(Map[Asset, Long](
              Waves -> matcherFee,
              issuedAsset -> oneOrderAmount
            ))
          }
          mkOrder(account, assetPair, OrderType.SELL, oneOrderAmount, orderPrice, ts = now + i)
      }

      sells.foreach(placeAndAwaitAtDex(_))

      info("checking there is no tradable Waves on accounts")
      accountsAndAssets.foreach { case (account, asset) =>
        val assetPair = AssetPair(IssuedAsset(asset.id()), Waves)
        eventually {
          dex1.api.getTradableBalance(account, assetPair) should matchTo(Map.empty[Asset, Long])
        }
      }

      info("placing buy orders (10 for each sell)")
      val submittedOrdersNumber = 10
      val buyOrders =
        for {
          (_, asset) <- accountsAndAssets
          i <- 1 to submittedOrdersNumber
        } yield mkOrder(
          alice,
          AssetPair(IssuedAsset(asset.id()), Waves),
          OrderType.BUY,
          amount = oneOrderAmount / submittedOrdersNumber,
          price = orderPrice,
          ts = now + i
        )

      Await.ready(
        Future.traverse(buyOrders.groupBy(_.assetPair).values) { orders =>
          Future.inSeries(orders)(dex2.asyncApi.place(_))
        },
        5.minutes
      )

      info("checking that order weren't canceled")
      val firstCanceled = sells.view
        .map { order =>
          order.idStr() -> dex1.api.waitForOrder(order)(r => r.status == Status.Filled || r.status == Status.Cancelled).status
        }
        .collectFirst {
          case (id, Status.Cancelled) => id
        }

      firstCanceled.foreach { id =>
        fail(s"$id is canceled")
      }

      sells.foreach { o =>
        dex1.api.waitForOrderStatus(o, Status.Filled)
        dex2.api.waitForOrderStatus(o, Status.Filled)
      }

      wavesNode1.api.waitForHeightArise()

      info("post-checks")
      withClue("transactions check:\n") {
        sells.foreach { o =>
          withClue(s"oId=${o.id()}: ") {
            val txs1: List[ExchangeTransaction] = withClue("dex1: ") {
              val xs = dex1.api.getTransactionsByOrder(o)
              xs should have length submittedOrdersNumber

              xs.foreach { tx =>
                withClue(s"txId=${tx.id()}: ") {
                  eventually {
                    wavesNode1.tryApi.transactionInfo(tx.id()) shouldBe a[Right[_, _]]
                  }
                }
              }
              xs
            }

            val txs2: List[ExchangeTransaction] = withClue("dex2: ") {
              val xs = dex2.api.getTransactionsByOrder(o)
              xs should have length submittedOrdersNumber
              xs
            }

            txs1 should matchTo(txs2)
          }
        }
      }

      withClue("balances check:\n") {
        accountsAndAssets.foreach { case (account, issueTx) =>
          withClue(s"Asset: ${issueTx.id()}") {
            withClue(s"Account: $account") {
              eventually {
                wavesNode1.api.balance(account, IssuedAsset(issueTx.id())) shouldBe 0L
              }
            }

            withClue(s"Account: $account") {
              eventually {
                wavesNode1.api.balance(alice, IssuedAsset(issueTx.id())) shouldBe 10000L
              }
            }
          }
        }
      }
    }
  }
}
