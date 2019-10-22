package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.model.MatcherModel.Price
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{MatcherError, OrderStatus}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.SELL
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

class TradersTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config =
    ConfigFactory.parseString(s"""waves.dex.price-assets = [ "Aqy7PRU", "$UsdId", "WAVES" ]""".stripMargin)

  val orderVersions: Seq[Byte] = Seq(1, 2, 3)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueWctTx)
  }

  private def bobPlacesSellWctOrder(bobCoinAmount: Int, orderVersion: Byte): Order = {
    val r = mkOrder(bob, wctUsdPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, version = orderVersion)
    placeAndAwait(r)
    r
  }

  private def bobPlacesBuyWaveOrder(assetPair: AssetPair, amount: Long, price: Price): Order = {
    val r = mkOrder(bob, assetPair, OrderType.BUY, amount, price)
    placeAndAwait(r)
    r
  }

  "Verifications of tricky ordering cases" - {
    "AssetPair BOB/WAVES vs BOB/NULL" in {

      val trickyBobWavesPairWB58 = AssetPair(
        amountAsset = wct,
        priceAsset = IssuedAsset { ByteStr.decodeBase58("WAVES").get }
      )

      trickyBobWavesPairWB58.key shouldBe wctWavesPair.key

      val trickyBobWavesPairWS = AssetPair(
        amountAsset = wct,
        priceAsset = IssuedAsset { ByteStr("WAVES".getBytes) }
      )

      val trickyBobOrderWB58 = mkOrder(bob, trickyBobWavesPairWB58, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
      dex1Api.tryPlace(trickyBobOrderWB58) should failWith(9440512) // OrderInvalidSignature

      val trickyBobOrderWS = mkOrder(bob, trickyBobWavesPairWS, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
      dex1Api.tryPlace(trickyBobOrderWS) should failWith(
        11534345, // AssetNotFound
        MatcherError.Params(assetId = Some(trickyBobWavesPairWS.priceAssetStr))
      )

      val correctBobOrder = mkOrder(bob, wctWavesPair, OrderType.BUY, 1, 10.waves * Order.PriceConstant)
      placeAndAwait(correctBobOrder)

      val markets = dex1Api.allOrderBooks.markets.map(x => s"${x.amountAsset}-${x.priceAsset}").toSet

      withClue("hasTrickyBobWavesPairWB58Market\n") {
        markets.contains(trickyBobWavesPairWB58.key) shouldBe true
      }

      withClue("hasTrickyBobWavesPairWSMarket\n") {
        markets.contains(trickyBobWavesPairWS.key) shouldBe false
      }

      withClue("wctWavesPair\n") {
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

          for (orderV <- orderVersions) {
            val orderAmount              = 4000
            val transferAmount           = IssueWctTx.quantity - orderAmount
            val oldestOrder, newestOrder = bobPlacesSellWctOrder(orderAmount, orderV)

            // Transfer all coins except required for one order
            val transferTx = mkTransfer(bob, alice, transferAmount, wct)
            wavesNode1Api.broadcast(transferTx)

            withClue(s"The newest order of version $orderV '${newestOrder.idStr()}' was cancelled\n") {
              dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
            }

            withClue(s"The oldest order of version $orderV '${oldestOrder.idStr()}' is still active\n") {
              dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
            }

            withClue("Cleanup\n") {
              wavesNode1Api.waitForTransaction(transferTx)
              dex1Api.cancel(bob, oldestOrder)
              dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
              broadcastAndAwait(mkTransfer(alice, bob, transferAmount, wct))
            }
          }
        }

        "leased waves, insufficient fee" in {
          for (orderV <- orderVersions) {
            val bobBalance               = wavesNode1Api.balance(bob, Waves)
            val oldestOrder, newestOrder = bobPlacesSellWctOrder(1000, orderV)

            // Lease all waves except required for one order
            val leaseAmount = bobBalance - matcherFee - leasingFee
            val lease       = mkLease(bob, alice, leaseAmount, leasingFee)

            wavesNode1Api.broadcast(lease)

            withClue(s"The newest order of version $orderV '${newestOrder.idStr()}' was cancelled") {
              dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
            }

            withClue(s"The oldest order of version $orderV '${oldestOrder.idStr()}' is still active") {
              dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
            }

            withClue("Cleanup") {
              wavesNode1Api.waitForTransaction(lease)
              dex1Api.cancel(bob, oldestOrder)
              dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
              broadcastAndAwait(mkLeaseCancel(bob, lease.id.value))
            }
          }
        }

        "moved waves, insufficient fee" in {
          for (orderV <- orderVersions) {
            val bobBalance               = wavesNode1Api.balance(bob, Waves)
            val oldestOrder, newestOrder = bobPlacesSellWctOrder(1000, orderV)

            // Transfer all waves except required for one order
            val transferAmount = bobBalance - matcherFee - minFee
            val transferTx     = mkTransfer(bob, alice, transferAmount, Waves, minFee)

            wavesNode1Api.broadcast(transferTx)

            withClue(s"The newest order of version $orderV '${newestOrder.idStr()}' was cancelled") {
              dex1Api.waitForOrderStatus(newestOrder, OrderStatus.Cancelled)
            }

            withClue(s"The oldest order of version $orderV '${oldestOrder.idStr()}' is still active") {
              dex1Api.orderStatus(oldestOrder).status shouldBe OrderStatus.Accepted
            }

            withClue("Cleanup") {
              wavesNode1Api.waitForTransaction(transferTx)
              dex1Api.cancel(bob, oldestOrder)
              dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
              broadcastAndAwait(mkTransfer(alice, bob, transferAmount, Waves))
            }
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
            wavesNode1Api.waitForTransaction(lease)
            dex1Api.cancel(bob, oldestOrder)
            dex1Api.waitForOrderStatus(oldestOrder, OrderStatus.Cancelled)
            broadcastAndAwait(mkLeaseCancel(bob, lease.id()))
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

          withClue("Cleanup") {
            wavesNode1Api.waitForTransaction(lease)
            broadcastAndAwait(mkLeaseCancel(bob, lease.id()))
          }
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

          withClue("Cleanup") {
            broadcastAndAwait(mkTransfer(alice, bob, transferAmount, Waves, matcherFee))
          }
        }

        "moved feeAsset, fee asset doesn't take part in trading pair" in {

          val bobAssetQuantity = 10000

          val newFeeAssetTx = mkIssue(bob, "FeeCoin", bobAssetQuantity, 2, issueFee)
          val newFeeAsset   = IssuedAsset(newFeeAssetTx.id())

          broadcastAndAwait(newFeeAssetTx)
          dex1Api.upsertRate(newFeeAsset, 2)

          val bobOrder = mkOrder(bob, wctUsdPair, SELL, 400L, 2 * 100000000L, matcherFee = 1, matcherFeeAssetId = newFeeAsset)

          dex1Api.place(bobOrder)
          dex1Api.reservedBalance(bob) shouldBe Map(wct -> 400, newFeeAsset -> 1)

          broadcastAndAwait(mkTransfer(bob, alice, bobAssetQuantity, newFeeAsset, matcherFee))

          withClue(s"The order '${bobOrder.idStr()}' was cancelled") {
            dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Cancelled)
          }
        }
      }
    }
  }
}
