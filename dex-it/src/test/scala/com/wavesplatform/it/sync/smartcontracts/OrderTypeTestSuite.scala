package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{MatcherError, OrderStatus}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

class OrderTypeTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")

  private val issueAliceAssetTx = mkIssue(alice, "AliceCoinOrders", someAssetAmount, decimals = 0)
  private val aliceAsset        = IssuedAsset(issueAliceAssetTx.id())

  private val predefAssetPair = wavesUsdPair
  private val aliceWavesPair  = AssetPair(aliceAsset, Waves)

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(issueAliceAssetTx, IssueUsdTx)
    startAndWait(dex1Container(), dex1Api)
  }

  "Order types verification with SmartContracts" - {
    val sco1 = s"""
                 |{-# STDLIB_VERSION 2 #-}
                 |match tx {
                 | case o : Order =>
                 |   o.orderType == Buy
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sco2 = s"""
              |{-# STDLIB_VERSION 2 #-}
              |match tx {
              | case o : Order =>
              |    o.orderType == Sell
              |  case s : SetScriptTransaction => true
              |  case _ => throw()
              | }
      """.stripMargin

    val sco3 = s"""
                 |{-# STDLIB_VERSION 2 #-}
                 |match tx {
                 |  case o : Order =>
                 |        o.orderType == Buy || o.orderType == Sell
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "scenarios of order placement" - {
      "set contracts with only BUY type and then place order" in {
        setAliceScriptText(sco1)

        val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwait(aliceOrd1)

        dex1Api.tryPlace(mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
          3147522, // AccountScriptDeniedOrder
          MatcherError.Params(address = Some(alice.toAddress.stringRepr))
        )

        dex1Api.cancel(alice, aliceOrd1).status shouldBe "OrderCanceled"
        resetAliceAccountScript()
      }

      "set contracts with only SELL type and then place order" in {
        setAliceScriptText(sco2)

        dex1Api.tryPlace(mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
          3147522, // AccountScriptDeniedOrder
          MatcherError.Params(address = Some(alice.toAddress.stringRepr))
        )

        val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwait(aliceOrd2)

        dex1Api.cancel(alice, aliceOrd2).status shouldBe "OrderCanceled"
        resetAliceAccountScript()
      }

      "set contracts with both SELL/BUY types and then place order" in {
        setAliceScriptText(sco3)

        val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwait(aliceOrd1)

        val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwait(aliceOrd2)

        dex1Api.cancel(alice, aliceOrd1).status shouldBe "OrderCanceled"
        dex1Api.cancel(alice, aliceOrd2).status shouldBe "OrderCanceled"
        resetAliceAccountScript()
      }

      "place order and then set contract on BUY type" in {
        val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwait(aliceOrd1)

        val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
        placeAndAwait(aliceOrd2)

        setAliceScriptText(sco1)

        val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 1)
        dex1Api.place(bobOrd1)

        val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 1)
        dex1Api.place(bobOrd2)

        dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Filled)
        dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Filled)
        dex1Api.waitForOrderStatus(bobOrd1, OrderStatus.Filled)
        dex1Api.waitForOrderStatus(bobOrd2, OrderStatus.Filled)

        waitForOrderAtNode(bobOrd1)

        val txs = dex1Api.waitForTransactionsByOrder(bobOrd2, 1)
        val r   = wavesNode1Api.tryBroadcast(txs.head)
        r shouldBe 'left
        r.left.get.error shouldBe TransactionNotAllowedByAccountScript.ErrorCode
      }
    }
  }

  private def setAliceScriptText(scriptText: String): Unit = broadcastAndAwait(mkSetAccountScriptText(alice, Some(scriptText)))
  private def resetAliceAccountScript(): Unit              = broadcastAndAwait(mkSetAccountScriptText(alice, None, fee = setScriptFee + smartFee))
}
