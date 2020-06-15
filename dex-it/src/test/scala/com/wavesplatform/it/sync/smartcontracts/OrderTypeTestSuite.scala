package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.it.MatcherSuiteBase

class OrderTypeTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")

  private val issueAliceAssetTx = mkIssue(alice, "AliceCoinOrders", someAssetAmount, decimals = 0)
  private val aliceAsset        = IssuedAsset(issueAliceAssetTx.getId)

  private val predefAssetPair = wavesUsdPair
  private val aliceWavesPair  = AssetPair(aliceAsset, Waves)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(issueAliceAssetTx, IssueUsdTx)
    dex1.start()
  }

  "Order types verification with SmartContracts" - {
    /*
    {-# STDLIB_VERSION 2 #-}
    match tx {
      case o : Order => o.orderType == Buy
      case s : SetScriptTransaction => true
      case other => throw()
    }
     */
    val sco1 = "AgQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAAFvBQAAAAckbWF0Y2gwCQAAAAAAAAII" +
      "BQAAAAFvAAAACW9yZGVyVHlwZQUAAAADQnV5AwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAABRTZXRTY3JpcHRUcmFuc2FjdGlvbgQAAAABcwUAAAAHJ" +
      "G1hdGNoMAYEAAAABW90aGVyBQAAAAckbWF0Y2gwCQEAAAAFdGhyb3cAAAAADXx9DQ=="

    "scenarios of order placement" - {
      "set contracts with only BUY type and then place order" in {
        setAliceScriptText(sco1)

        val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwaitAtDex(aliceOrd1)

        dex1.api.tryPlace(mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
          3147522, // AccountScriptDeniedOrder
          MatcherError.Params(address = Some(alice.toAddress.stringRepr))
        )

        dex1.api.cancel(alice, aliceOrd1).status shouldBe "OrderCanceled"
        resetAliceAccountScript()
      }

      "set contracts with only SELL type and then place order" in {
        /*
        {-# STDLIB_VERSION 2 #-}
        match tx {
          case o : Order => o.orderType == Sell
          case s : SetScriptTransaction => true
          case _ => throw()
        }
         */
        setAliceScriptText(
          "AgQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAAFvBQAAAAckbWF0Y2gwCQAAAAAAAAII" +
            "BQAAAAFvAAAACW9yZGVyVHlwZQUAAAAEU2VsbAMJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAAUU2V0U2NyaXB0VHJhbnNhY3Rpb24EAAAAAXMFAAAAB" +
            "yRtYXRjaDAGCQEAAAAFdGhyb3cAAAAAYWVPjA=="
        )

        dex1.api.tryPlace(mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
          3147522, // AccountScriptDeniedOrder
          MatcherError.Params(address = Some(alice.toAddress.stringRepr))
        )

        val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwaitAtDex(aliceOrd2)

        dex1.api.cancel(alice, aliceOrd2).status shouldBe "OrderCanceled"
        resetAliceAccountScript()
      }

      "set contracts with both SELL/BUY types and then place order" in {
        /*
        {-# STDLIB_VERSION 2 #-}
        match tx {
          case o : Order => o.orderType == Buy || o.orderType == Sell
          case s : SetScriptTransaction => true
          case _ => throw()
        }
         */
        setAliceScriptText(
          "AgQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAAFvBQAAAAckbWF0Y2gwAwkAAAAAAAACCAUAAAABb" +
            "wAAAAlvcmRlclR5cGUFAAAAA0J1eQYJAAAAAAAAAggFAAAAAW8AAAAJb3JkZXJUeXBlBQAAAARTZWxsAwkAAAEAAAACBQAAAAckbWF0Y2gwAgAA" +
            "ABRTZXRTY3JpcHRUcmFuc2FjdGlvbgQAAAABcwUAAAAHJG1hdGNoMAYJAQAAAAV0aHJvdwAAAAAeB1+u"
        )

        val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwaitAtDex(aliceOrd1)

        val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwaitAtDex(aliceOrd2)

        dex1.api.cancel(alice, aliceOrd1).status shouldBe "OrderCanceled"
        dex1.api.cancel(alice, aliceOrd2).status shouldBe "OrderCanceled"
        resetAliceAccountScript()
      }

      "place order and then set contract on BUY type" in {
        val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwaitAtDex(aliceOrd1)

        val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwaitAtDex(aliceOrd2)

        setAliceScriptText(sco1)

        val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 1)
        dex1.api.place(bobOrd1)

        val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 1)
        dex1.api.place(bobOrd2)

        dex1.api.waitForOrderStatus(aliceOrd1, Status.Filled)
        dex1.api.waitForOrderStatus(aliceOrd2, Status.Filled)
        dex1.api.waitForOrderStatus(bobOrd1, Status.Filled)
        dex1.api.waitForOrderStatus(bobOrd2, Status.Filled)

        waitForOrderAtNode(bobOrd1)

        val txs = dex1.api.waitForTransactionsByOrder(bobOrd2, 1)
        val r   = wavesNode1.api.tryBroadcast(txs.head)
        r shouldBe Symbol("left")
        r.left.get.error shouldBe 307 // node's ApiError TransactionNotAllowedByAccountScript.Id
      }
    }
  }

  private def setAliceScriptText(binaryCodeInBase64: String): Unit =
    broadcastAndAwait(mkSetAccountScript(alice, Scripts.fromBase64(binaryCodeInBase64)))
  private def resetAliceAccountScript(): Unit = broadcastAndAwait(mkResetAccountScript(alice, fee = setScriptFee + smartFee))
}
