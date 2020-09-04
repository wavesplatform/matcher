package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.effect.FutureOps.Implicits
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItExternalKafkaRequired
import com.wavesplatform.wavesj.Transfer

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

@DexItExternalKafkaRequired
class AutoCancelOrderTestSuite extends MatcherSuiteBase {

  override protected val wavesNodeInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  utx.ignore-exchange-sender-pk-in-pessimistic-portfolio = "${matcher.publicKey.base58}"
       |}""".stripMargin
  )

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

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
      val orderBook = dex1.api.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Auto cancel" - {
    "wrong cancel when match orders on all coins" in {
      val accounts = (1 to 5).map(_ => createAccountWithBalance(issueFee -> Waves))
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

      val submittedOrdersNumber = 10
      val buyOrders = for {
        (_, asset) <- accountsAndAssets
        i          <- 1 to submittedOrdersNumber
      } yield
        mkOrder(
          alice,
          AssetPair(IssuedAsset(asset.getId), Waves),
          OrderType.BUY,
          amount = oneOrderAmount / submittedOrdersNumber,
          price = orderPrice,
          ttl = 1.day + i.minutes
        )

      Await.ready(
        {
          Future.traverse(buyOrders.groupBy(_.assetPair).values) { orders =>
            Future.inSeries(orders)(dex2.asyncApi.place(_))
          }
        },
        5.minutes
      )

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
    }
  }
}
