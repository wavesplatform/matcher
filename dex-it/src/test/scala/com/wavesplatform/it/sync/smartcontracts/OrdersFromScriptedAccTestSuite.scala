package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

class OrdersFromScriptedAccTestSuite extends NewMatcherSuiteBase {
  private val activationHeight = 4
  override protected val suiteInitialWavesNodeConfig: Config = ConfigFactory.parseString(
    s"""waves {
       |  utx.allow-skip-checks = false
       |
       |  blockchain.custom.functionality.pre-activated-features = {
       |    ${BlockchainFeatures.SmartAccountTrading.id} = $activationHeight,
       |    ${BlockchainFeatures.SmartAssets.id} = 1000
       |  }
       |}""".stripMargin
  )

  private val aliceAssetTx   = mk(alice, "AliceCoin", someAssetAmount, 0)
  private val aliceAsset     = IssuedAsset(aliceAssetTx.id())
  private val aliceWavesPair = AssetPair(aliceAsset, Waves)

  private def updateBobScript(codeText: String): Unit = broadcastAndAwait(mkSetAccountScriptText(bob, Some(codeText), fee = setScriptFee + smartFee))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(aliceAssetTx, mkSetAccountScriptText(bob, Some("true")))
  }

  "issue asset and run test" - {
    "trading is deprecated" in {
      dex1Api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 1)) should failWith(
        2097923,
        "An account's feature isn't yet supported"
      )
    }

    "can't place an OrderV2 before the activation" in {
      dex1Api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)) should failWith(
        2099459,
        "The order of version 2 isn't yet supported"
      )
    }

    "invalid setScript at account" in {
      wavesNode1Api.waitForHeight(activationHeight)
      updateBobScript("true && (height > 0)")
      dex1Api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)) should failWith(
        3147521,
        "An access to the blockchain.height is denied on DEX"
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      updateBobScript(
        """let x = (let x = 2
          |3)
          |x == 3""".stripMargin
      )
      dex1Api
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
      dex1Api.place(bobOrder).status shouldBe "OrderAccepted"
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
        dex1Api
          .place(mkOrder(bob, aliceWavesPair, OrderType.BUY, 2000, 2.waves * Order.PriceConstant, smartTradeFee, version = 2))
          .status shouldBe "OrderAccepted"
      }

      "reject incorrect order" in {
        dex1Api.tryPlace(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)) should failWith(
          3147522
        )
      }
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1)
      dex1Api.place(aliceOrder).status shouldBe "OrderAccepted"

      // Alice checks that the order in order book
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)
      dex1Api.orderHistory(alice).head.status shouldBe OrderStatus.Filled
    }
  }
}
