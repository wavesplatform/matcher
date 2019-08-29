package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.model.MatcherModel.Price
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.{MatcherError, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

class TradersTestSuite extends NewMatcherSuiteBase {

  override protected def dex1Config: Config =
    ConfigFactory.parseString("waves.dex.allowed-order-versions = [1, 2, 3]").withFallback(super.dex1Config)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueUsdTx, IssueWctTx)
  }

  "Verifications of tricky ordering cases" - {
    "AssetPair BOB/WAVES vs BOB/NULL" in {
      val trickyBobWavesPairWB58 = AssetPair(
        amountAsset = WctAsset,
        priceAsset = IssuedAsset(ByteStr.decodeBase58("WAVES").get)
      )

      trickyBobWavesPairWB58.key shouldBe wctWavesPair.key

      val trickyBobWavesPairWS = AssetPair(
        amountAsset = WctAsset,
        priceAsset = IssuedAsset(ByteStr("WAVES".getBytes()))
      )

      val trickyBobOrderWB58 = mkOrder(bob, matcher, trickyBobWavesPairWB58, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
      dex1Api.tryPlace(trickyBobOrderWB58) should failWith(9440512) // OrderInvalidSignature

      val trickyBobOrderWS = mkOrder(bob, matcher, trickyBobWavesPairWS, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
      dex1Api.tryPlace(trickyBobOrderWS) should failWith(
        11534345,
        MatcherError.Params(assetId = Some(trickyBobWavesPairWS.priceAssetStr)) // AssetNotFound
      )

      val correctBobOrder = mkOrder(bob, matcher, wctWavesPair, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
      dex1Api.place(correctBobOrder)
      dex1Api.waitForOrderStatus(correctBobOrder, OrderStatus.Accepted)

      val markets = dex1Api.allOrderBooks.markets.map(x => s"${x.amountAsset}-${x.priceAsset}").toSet

      withClue("hasTrickyBobWavesPairWB58Market") {
        markets.contains(trickyBobWavesPairWB58.key) shouldBe true
      }

      withClue("hasTrickyBobWavesPairWSMarket") {
        markets.contains(trickyBobWavesPairWS.key) shouldBe false
      }

      withClue("wctWavesPair") {
        markets.contains(wctWavesPair.key) shouldBe true
      }

      withClue("Cleanup") {
        dex1Api.orderBook(wctWavesPair).bids shouldNot be(empty)
        dex1Api.cancel(bob, correctBobOrder)
        dex1Api.waitForOrderStatus(correctBobOrder, OrderStatus.Cancelled)
      }
    }

    "owner moves assets/waves to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val orderAmount              = 4000
          val transferAmount           = IssueWctTx.quantity - 100
          val oldestOrder, newestOrder = bobPlacesSellWctOrder(orderAmount)

          // Transfer all coins except required for one order
          wavesNode1Api.broadcast(mkTransfer(bob, alice, transferAmount, WctAsset))

          withClue(s"The newest order '${newestOrder.idStr()}' was cancelled") {
            dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
          }
          withClue(s"The oldest order '${oldestOrder.idStr()}' is still active") {
            dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
          }

          withClue("Cleanup") {
            dex1Api.cancel(bob, oldestOrder)
            dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
            wavesNode1Api.broadcast(mkTransfer(alice, bob, transferAmount, WctAsset))
          }
        }

        "leased waves, insufficient fee" in {
          val bobBalance               = wavesNode1Api.balance(bob, Waves)
          val oldestOrder, newestOrder = bobPlacesSellWctOrder(1000)

          // Lease all waves except required for one order
          val leaseAmount = bobBalance - matcherFee - leasingFee
          val lease       = mkLease(bob, alice, leaseAmount, leasingFee)
          wavesNode1Api.broadcast(lease)

          withClue(s"The newest order '${newestOrder.idStr()}' was cancelled") {
            dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
          }
          withClue(s"The oldest order '${oldestOrder.idStr()}' is still active") {
            dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
          }

          withClue("Cleanup") {
            dex1Api.cancel(bob, oldestOrder)
            dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
            wavesNode1Api.broadcast(mkLeaseCancel(bob, lease.id()))
          }
        }

        "moved waves, insufficient fee" in {
          val bobBalance               = wavesNode1Api.balance(bob, Waves)
          val oldestOrder, newestOrder = bobPlacesSellWctOrder(1000)

          // Transfer all waves except required for one order
          val transferAmount = bobBalance - matcherFee - minFee
          wavesNode1Api.broadcast(mkTransfer(bob, alice, transferAmount, Waves, minFee))

          withClue(s"The newest order '${newestOrder.idStr()}' was cancelled") {
            dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
          }
          withClue(s"The oldest order '${oldestOrder.idStr()}' is still active") {
            dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
          }

          withClue("Cleanup") {
            dex1Api.cancel(bob, oldestOrder)
            dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
            wavesNode1Api.broadcast(mkTransfer(alice, bob, transferAmount, Waves))
          }
        }
      }

      "order with waves" - {
        "leased waves, insufficient fee for one ExchangeTransaction" in {
          // Amount of waves in order is smaller than fee
          val bobBalance               = wavesNode1Api.balance(bob, Waves)
          val oldestOrder, newestOrder = bobPlacesBuyWaveOrder(wctWavesPair, 1, 10.waves * Order.PriceConstant)

          // Lease all waves except required for one order
          val leaseAmount = bobBalance - matcherFee - 10.waves - leasingFee // TODO ???
          val lease       = mkLease(bob, alice, leaseAmount, leasingFee)
          wavesNode1Api.broadcast(lease)

          withClue(s"The newest order '${newestOrder.idStr()}' is Cancelled") {
            dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
          }
          withClue(s"The oldest order '${oldestOrder.idStr()}' is still active") {
            dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
          }

          withClue("Cleanup") {
            dex1Api.cancel(bob, oldestOrder)
            dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
            wavesNode1Api.broadcast(mkLeaseCancel(bob, lease.id()))
          }
        }

        "leased waves, insufficient waves" in {
          val bobBalance = wavesNode1Api.balance(bob, Waves)
          val price      = 1.waves
          val order      = bobPlacesBuyWaveOrder(wctWavesPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - matcherFee - price / 2
          val lease       = mkLease(bob, alice, leaseAmount, leasingFee)
          wavesNode1Api.broadcast(lease)

          withClue(s"The order '${order.idStr()}' was cancelled") {
            dex1Api.waitForOrderStatus(order, OrderStatus.Cancelled)
          }

          withClue("Cleanup")(wavesNode1Api.broadcast(mkLeaseCancel(bob, lease.id())))
        }

        "moved waves, insufficient fee" in {
          // Amount of waves in order is smaller than fee
          val bobBalance = wavesNode1Api.balance(bob, Waves)
          val price      = matcherFee / 2
          val order      = bobPlacesBuyWaveOrder(wctWavesPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - matcherFee - price
          wavesNode1Api.broadcast(mkTransfer(bob, alice, transferAmount, Waves, matcherFee)) // TODO fee

          withClue(s"The order '${order.idStr()}' was cancelled") {
            dex1Api.waitForOrderStatus(order, OrderStatus.Cancelled)
          }

          withClue("Cleanup")(wavesNode1Api.broadcast(mkTransfer(alice, bob, transferAmount, Waves, matcherFee)))
        }
      }
    }
  }

  private def bobPlacesBuyWaveOrder(assetPair: AssetPair, amount: Long, price: Price): Order = {
    val r = mkOrder(bob, matcher, assetPair, OrderType.BUY, amount, price)
    dex1Api.place(r)
    dex1Api.waitForOrderStatus(r, OrderStatus.Accepted)
    r
  }

  private def bobPlacesSellWctOrder(bobCoinAmount: Int): Order = {
    val r = mkOrder(bob, matcher, wctUsdPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant)
    dex1Api.place(r)
    dex1Api.waitForOrderStatus(r, OrderStatus.Accepted)
    r
  }
}
