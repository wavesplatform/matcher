package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.feature.BlockchainFeatures
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.responses.node.ActivationStatusResponse.FeatureStatus.BlockchainStatus
import com.wavesplatform.dex.it.test.Scripts
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

  private val aliceAssetTx = mkIssue(alice, "AliceCoin", someAssetAmount, 0)
  private val aliceAsset = IssuedAsset(aliceAssetTx.id())
  private val aliceWavesPair = AssetPair(aliceAsset, Waves)

  private def updateBobScript(binaryCodeInBase64: String): Unit =
    broadcastAndAwait(mkSetAccountMayBeScript(bob, Some(Scripts.fromBase64(binaryCodeInBase64)), fee = setScriptFee + smartFee))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(aliceAssetTx, mkSetAccountScript(bob, Scripts.alwaysTrue))
  }

  "issue asset and run test" - {

    "trading is deprecated" in {
      dex1.tryApi.place(
        mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 1)
      ) should failWith(
        2097923, // AccountFeatureUnsupported
        "An account's feature isn't yet supported"
      )
    }

    "can't place an OrderV2 before the activation" in {
      dex1.tryApi.place(
        mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)
      ) should failWith(
        2099459, // OrderVersionUnsupported
        "The order of version 2 isn't yet supported"
      )
    }

    "invalid setScript at account" in {
      wavesNode1.api.waitForActivationStatus(_.features.exists { x =>
        x.id == BlockchainFeatures.SmartAccountTrading.id && x.blockchainStatus == BlockchainStatus.Activated
      })
      // true && (height > 0)
      updateBobScript("AgMGCQAAZgAAAAIFAAAABmhlaWdodAAAAAAAAAAAAAeEODpj")
      Thread.sleep(3000) // TODO Sometimes fail without this awaiting, probably issue in the cache
      dex1.tryApi.place(
        mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)
      ) should failWith(
        3147520, // AccountScriptReturnedError
        "An access to the blockchain.height is denied on DEX"
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      // let x = 3; x == 3
      updateBobScript("AgQAAAABeAAAAAAAAAAAAwkAAAAAAAACBQAAAAF4AAAAAAAAAAADT0BZng==")
      dex1.api
        .place(mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2))
        .status shouldBe "OrderAccepted"
    }

    "scripted dApp account can trade" in {
      /*
      {-# STDLIB_VERSION 3       #-}
      {-# CONTENT_TYPE   DAPP    #-}
      {-# SCRIPT_TYPE    ACCOUNT #-}

      @Callable(i)
      func call() = WriteSet([])
       */
      updateBobScript("AAIDAAAAAAAAAAQIARIAAAAAAAAAAAEAAAABaQEAAAAEY2FsbAAAAAAJAQAAAAhXcml0ZVNldAAAAAEFAAAAA25pbAAAAABTiKBL")

      val bobOrder = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)
      dex1.api.place(bobOrder).status shouldBe "OrderAccepted"
    }

    "scripted dApp account" - {
      /*
      {-# STDLIB_VERSION 3       #-}
      {-# CONTENT_TYPE   DAPP    #-}
      {-# SCRIPT_TYPE    ACCOUNT #-}

      @Verifier(tx)
      func verify() =
      match tx {
        case o: Order => o.amount > 1000
        case _=> true
      }
       */
      "prepare" in updateBobScript(
        "AAIDAAAAAAAAAAIIAQAAAAAAAAAAAAAAAQAAAAJ0eAEAAAAGdmVyaWZ5AAAAAAQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAABy" +
        "RtYXRjaDACAAAABU9yZGVyBAAAAAFvBQAAAAckbWF0Y2gwCQAAZgAAAAIIBQAAAAFvAAAABmFtb3VudAAAAAAAAAAD6AboNesg"
      )

      "accept correct order" in {
        dex1.api
          .place(mkOrder(bob, aliceWavesPair, OrderType.BUY, 2000, 2.waves * Order.PriceConstant, smartTradeFee, version = 2))
          .status shouldBe "OrderAccepted"
      }

      "reject incorrect order" in {
        dex1.tryApi.place(
          mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartTradeFee, version = 2)
        ) should failWith(
          3147522 // AccountScriptDeniedOrder
        )
      }
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, version = 1)
      dex1.api.place(aliceOrder).status shouldBe "OrderAccepted"

      // Alice checks that the order in order book
      dex1.api.waitForOrderStatus(aliceOrder, Status.Filled)
      dex1.api.getOrderHistoryByPublicKey(alice).head.status shouldBe Status.Filled.name
    }

    "zero transfers inside dapp should affect balances" in {

      /**
       * Script:
       *  {-# STDLIB_VERSION 4 #-}
       *  {-# CONTENT_TYPE DAPP #-}
       *  {-# SCRIPT_TYPE ACCOUNT #-}
       *
       *  @Callable(i)
       *  func call() = {
       *    [
       *      ScriptTransfer(Address(base58'3Q6WsHs7d2EndK5DBcFPRioRSkUyzWq2Bfo'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Q93DhCrhAJ58jTRJkpYaQcCC5MXwCJBZcs'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Ptyk3pMcDqD74rjCgowy7cdHmaDnru8yra'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Q6WsHs7d2EndK5DBcFPRioRSkUyzWq2Bfo'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Q93DhCrhAJ58jTRJkpYaQcCC5MXwCJBZcs'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Ptyk3pMcDqD74rjCgowy7cdHmaDnru8yra'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Q6WsHs7d2EndK5DBcFPRioRSkUyzWq2Bfo'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Q93DhCrhAJ58jTRJkpYaQcCC5MXwCJBZcs'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Ptyk3pMcDqD74rjCgowy7cdHmaDnru8yra'), 0, base58'WAVES'),
       *      ScriptTransfer(Address(base58'3Q6WsHs7d2EndK5DBcFPRioRSkUyzWq2Bfo'), 0, base58'WAVES')
       *    ]
       *  }
       *
       *  @Verifier(tx)
       *  func verify() = match tx { case o: Order => o.amount > 1 case _=> true }
       */
      val dapp = mkAccountWithBalance(100.waves + setScriptFee + smartFee -> Waves)

      val script =
        "AAIEAAAAAAAAAAQIAhIAAAAAAAAAAAEAAAABaQEAAAAEY2FsbAAAAAAJAARMAAAAAgkBAAAADlNjcmlwdFRyYW5zZmVyAA" +
        "AAAwkBAAAAB0FkZHJlc3MAAAABAQAAABoBWa6EvcHADfNgGRp2CUQu/MdCzv2ovxuZugAAAAAAAAAAAAEAAAAEE6vZMw" +
        "kABEwAAAACCQEAAAAOU2NyaXB0VHJhbnNmZXIAAAADCQEAAAAHQWRkcmVzcwAAAAEBAAAAGgFZyjKAs7B9YcXPkEu6sq" +
        "qaLmcjbJpVCNqgAAAAAAAAAAAAAQAAAAQTq9kzCQAETAAAAAIJAQAAAA5TY3JpcHRUcmFuc2ZlcgAAAAMJAQAAAAdBZG" +
        "RyZXNzAAAAAQEAAAAaAVkv+GrVSVOgndNAZqJ6D774k7OshG5mbVMAAAAAAAAAAAABAAAABBOr2TMJAARMAAAAAgkBAA" +
        "AADlNjcmlwdFRyYW5zZmVyAAAAAwkBAAAAB0FkZHJlc3MAAAABAQAAABoBWa6EvcHADfNgGRp2CUQu/MdCzv2ovxuZug" +
        "AAAAAAAAAAAAEAAAAEE6vZMwkABEwAAAACCQEAAAAOU2NyaXB0VHJhbnNmZXIAAAADCQEAAAAHQWRkcmVzcwAAAAEBAA" +
        "AAGgFZyjKAs7B9YcXPkEu6sqqaLmcjbJpVCNqgAAAAAAAAAAAAAQAAAAQTq9kzCQAETAAAAAIJAQAAAA5TY3JpcHRUcm" +
        "Fuc2ZlcgAAAAMJAQAAAAdBZGRyZXNzAAAAAQEAAAAaAVkv+GrVSVOgndNAZqJ6D774k7OshG5mbVMAAAAAAAAAAAABAA" +
        "AABBOr2TMJAARMAAAAAgkBAAAADlNjcmlwdFRyYW5zZmVyAAAAAwkBAAAAB0FkZHJlc3MAAAABAQAAABoBWa6EvcHADf" +
        "NgGRp2CUQu/MdCzv2ovxuZugAAAAAAAAAAAAEAAAAEE6vZMwkABEwAAAACCQEAAAAOU2NyaXB0VHJhbnNmZXIAAAADCQ" +
        "EAAAAHQWRkcmVzcwAAAAEBAAAAGgFZyjKAs7B9YcXPkEu6sqqaLmcjbJpVCNqgAAAAAAAAAAAAAQAAAAQTq9kzCQAETA" +
        "AAAAIJAQAAAA5TY3JpcHRUcmFuc2ZlcgAAAAMJAQAAAAdBZGRyZXNzAAAAAQEAAAAaAVkv+GrVSVOgndNAZqJ6D774k7" +
        "OshG5mbVMAAAAAAAAAAAABAAAABBOr2TMJAARMAAAAAgkBAAAADlNjcmlwdFRyYW5zZmVyAAAAAwkBAAAAB0FkZHJlc3" +
        "MAAAABAQAAABoBWa6EvcHADfNgGRp2CUQu/MdCzv2ovxuZugAAAAAAAAAAAAEAAAAEE6vZMwUAAAADbmlsAAAAAQAAAA" +
        "J0eAEAAAAGdmVyaWZ5AAAAAAQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBA" +
        "AAAAFvBQAAAAckbWF0Y2gwCQAAZgAAAAIIBQAAAAFvAAAABmFtb3VudAAAAAAAAAAAAQYOsYqv"

      broadcastAndAwait(mkSetAccountMayBeScript(dapp, Some(Scripts.fromBase64(script)), fee = setScriptFee + smartFee))

      val o = mkOrder(dapp, aliceWavesPair, BUY, 10, 1.waves, version = 3.toByte)
      placeAndAwaitAtDex(o) //dapp balance is 100.waves

      eventually {
        dex1.api.getTradableBalance(dapp, aliceWavesPair).getOrElse(Waves, 0L) shouldBe 9999699990L //89.997.waves ??
        dex1.api.getReservedBalance(dapp) shouldBe Map(Waves -> 300010L) //10.003.waves ??
      }

      cancelAndAwait(dapp, o)

      eventually {
        dex1.api.getTradableBalance(dapp, aliceWavesPair).getOrElse(Waves, 0L) shouldBe 100.waves
        dex1.api.getReservedBalance(dapp) shouldBe Map.empty
      }
    }
  }
}
