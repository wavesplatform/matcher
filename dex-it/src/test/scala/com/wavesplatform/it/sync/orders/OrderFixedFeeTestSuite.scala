package com.wavesplatform.it.sync.orders

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults

class OrderFixedFeeTestSuite extends OrderFeeBaseTestSuite {
  private val minMatcherFee = 200000L
  private val priceFixed    = 100000000L

  private val IssueResults(issueAliceAssetTx, _, aliceAsset) = mkIssueExtended(alice, "AliceCoin", quantity = 9999999999999L, decimals = 0)
  private val IssueResults(issueAliceScriptedAssetTx, _, aliceScriptedAsset) =
    mkIssueExtended(alice, "AliceSmartAsset", quantity = 9999999999999L, decimals = 0, fee = smartIssueFee, script = Some(Scripts.alwaysTrue))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(issueAliceAssetTx, issueAliceScriptedAssetTx)
  }

  "Matcher" - {
    "when has some asset as fixed fee in config and orders placed" - {
      "should accept orders if orders' matcherFeeAsset equal to specified in config" - {
        List(
          "regular asset"  -> aliceAsset,
          "scripted asset" -> aliceScriptedAsset
        ).foreach {
          case (title, asset) =>
            s"$title" in {
              val pair        = AssetPair(asset, Waves)
              val orderAmount = 1

              dex1.restartWithNewSuiteConfig { configWithOrderFeeFixed(asset) }
              broadcastAndAwait(mkTransfer(alice, bob, wavesNode1.api.balance(alice, asset) / 2, asset, feeAmount = minFee + smartFee))

              val aliceAssetBalanceBefore = wavesNode1.api.balance(alice, asset)
              val aliceWavesBalanceBefore = wavesNode1.api.balance(alice, Waves)

              val bobAssetBalanceBefore = wavesNode1.api.balance(bob, asset)
              val bobWavesBalanceBefore = wavesNode1.api.balance(bob, Waves)

              val aliceOrder = mkOrder(alice, pair, OrderType.BUY, orderAmount, priceFixed, matcherFee = minMatcherFee, feeAsset = asset)
              placeAndAwaitAtDex(aliceOrder)

              val reservedBalance1 = dex1.api.reservedBalance(alice)
              reservedBalance1(Waves) shouldBe orderAmount * priceFixed / 100000000
              reservedBalance1(asset) shouldBe minMatcherFee

              val tradableBalance1 = dex1.api.tradableBalance(alice, pair)
              tradableBalance1(Waves) shouldBe aliceWavesBalanceBefore - (orderAmount * priceFixed / 100000000)
              tradableBalance1(asset) shouldBe aliceAssetBalanceBefore - minMatcherFee

              val bobOrder = mkOrder(bob, pair, OrderType.SELL, orderAmount, priceFixed, matcherFee = minMatcherFee, feeAsset = asset)
              dex1.api.place(bobOrder)

              dex1.api.waitForOrderStatus(aliceOrder, Status.Filled)
              dex1.api.waitForOrderStatus(bobOrder, Status.Filled)

              waitForOrderAtNode(aliceOrder)

              wavesNode1.api.balance(alice, asset) shouldBe aliceAssetBalanceBefore - minMatcherFee + orderAmount
              wavesNode1.api.balance(alice, Waves) shouldBe aliceWavesBalanceBefore - (orderAmount * priceFixed / 100000000)

              wavesNode1.api.balance(bob, asset) shouldBe bobAssetBalanceBefore - minMatcherFee - orderAmount
              wavesNode1.api.balance(bob, Waves) shouldBe bobWavesBalanceBefore + (orderAmount * priceFixed / 100000000)
            }
        }
      }

      "should accept orders if sender received amount > than fee amount" in {
        dex1.restartWithNewSuiteConfig { configWithOrderFeeFixed(aliceAsset) }
        broadcastAndAwait(mkTransfer(bob, alice, minMatcherFee, aliceAsset))
        val pair = AssetPair(aliceAsset, Waves)
        placeAndAwaitAtDex(mkOrder(bob, pair, OrderType.BUY, amount = minMatcherFee, priceFixed, matcherFee = minMatcherFee, feeAsset = aliceAsset))
      }

      "should reject orders if orders' matcherFeeAsset not equal to specified in config" in {
        val pair = AssetPair(aliceAsset, Waves)
        dex1.api.tryPlace(mkOrder(alice, pair, OrderType.BUY, 1, priceFixed)) shouldBe Symbol("left")
        dex1.api.tryPlace(mkOrder(bob, pair, OrderType.SELL, 1, priceFixed)) shouldBe Symbol("left")
      }

      "should reject orders if orders' matcherFee less than specified minFee in config" in {
        val amount                 = 1
        val pair                   = AssetPair(aliceAsset, Waves)
        val insufficientMatcherFee = minMatcherFee - 1

        dex1.api.tryPlace(mkOrder(alice, pair, OrderType.BUY, amount, priceFixed, matcherFee = insufficientMatcherFee, feeAsset = aliceAsset)) should failWith(
          9441542, // FeeNotEnough
          s"Required $minMatcherFee ${aliceAsset.toString}"
        )

        dex1.api.tryPlace(mkOrder(bob, pair, OrderType.SELL, amount, priceFixed, matcherFee = insufficientMatcherFee, feeAsset = aliceAsset)) should failWith(
          9441542, // FeeNotEnough
          s"Required $minMatcherFee ${aliceAsset.toString}"
        )
      }
    }
  }

  private def configWithOrderFeeFixed(matcherFeeAsset: Asset): Config = {
    parseString(s"""waves.dex {
                   | allowed-order-versions = [1, 2, 3]
                   |  order-fee.-1 {
                   |    mode = fixed
                   |    fixed {
                   |      asset = ${matcherFeeAsset.toString}
                   |      min-fee = $minMatcherFee
                   |   }
                   |  }
                   |}""".stripMargin)
  }
}
