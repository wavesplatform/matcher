package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.feature.BlockchainFeatures
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.api.responses.node.ActivationStatusResponse.FeatureStatus.BlockchainStatus
import com.wavesplatform.it.MatcherSuiteBase

class OrdersFromScriptedAccTestSuite extends MatcherSuiteBase {

  private val activationHeight = 5

  override protected val wavesNodeInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves {
       |  miner.minimal-block-generation-offset = 10s
       |
       |  utx.allow-skip-checks = false
       |
       |  blockchain.custom.functionality.pre-activated-features = {
       |    ${BlockchainFeatures.SmartAccountTrading.id} = $activationHeight
       |    ${BlockchainFeatures.SmartAssets.id} = 1000
       |  }
       |}""".stripMargin
  )

  private val aliceAssetTx   = mkIssue(alice, "AliceCoin", someAssetAmount, 0)
  private val aliceAsset     = IssuedAsset(aliceAssetTx.getId)
  private val aliceWavesPair = AssetPair(aliceAsset, Waves)

  private def updateBobScript(codeText: String): Unit = broadcastAndAwait(mkSetAccountScript(bob, Some(codeText), fee = setScriptFee + smartFee))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(aliceAssetTx, mkSetAccountScript(bob, Some("true")))
  }

  "issue asset and run test" - {
    "trading is deprecated" in {
      dex1.api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 1)) should failWith(
        2097923, // AccountFeatureUnsupported
        "An account's feature isn't yet supported"
      )
    }

    "can't place an OrderV2 before the activation" in {
      dex1.api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)) should failWith(
        2099459, // OrderVersionUnsupported
        "The order of version 2 isn't yet supported"
      )
    }

    "invalid setScript at account" in {
      wavesNode1.api.waitForActivationStatus(_.features.exists { x =>
        x.id == BlockchainFeatures.SmartAccountTrading.id && x.blockchainStatus == BlockchainStatus.Activated
      })
      updateBobScript("true && (height > 0)")
      Thread.sleep(3000) // TODO Sometimes fail without this awaiting, probably issue in the cache
      dex1.api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)) should failWith(
        3147521, // AccountScriptException
        "An access to the blockchain.height is denied on DEX"
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      updateBobScript(
        """let x = (let x = 2
          |3)
          |x == 3""".stripMargin
      )
      dex1.api
        .place(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2))
        .status shouldBe "OrderAccepted"
    }

    "scripted dApp account can trade" in {
      updateBobScript(
        """{-# STDLIB_VERSION 3       #-}
          |{-# CONTENT_TYPE   DAPP    #-}
          |{-# SCRIPT_TYPE    ACCOUNT #-}
          |
          |@Callable(i)
          |func call() = WriteSet([])
          |""".stripMargin
      )

      val bobOrder = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)
      dex1.api.place(bobOrder).status shouldBe "OrderAccepted"
    }

    "scripted dApp account" - {
      "prepare" in updateBobScript(
        """{-# STDLIB_VERSION 3       #-}
          |{-# CONTENT_TYPE   DAPP    #-}
          |{-# SCRIPT_TYPE    ACCOUNT #-}
          |
          |@Verifier(tx)
          |func verify() =
          |  match tx {
          |    case o: Order => o.amount > 1000
          |    case _        => true
          |  }
          |""".stripMargin
      )

      "accept correct order" in {
        dex1.api
          .place(mkOrder(bob, aliceWavesPair, OrderType.BUY, 2000, 2.waves * Order.PriceConstant, smartTradeFee, version = 2))
          .status shouldBe "OrderAccepted"
      }

      "reject incorrect order" in {
        dex1.api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)) should failWith(
          3147522 // AccountScriptDeniedOrder
        )
      }
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1)
      dex1.api.place(aliceOrder).status shouldBe "OrderAccepted"

      // Alice checks that the order in order book
      dex1.api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)
      dex1.api.orderHistory(alice).head.status shouldBe OrderStatus.Filled
    }
  }
}
