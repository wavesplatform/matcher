//package com.wavesplatform.it.sync.smartcontracts
//
//import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
//import com.wavesplatform.common.state.ByteStr
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.it.sync._
//import com.wavesplatform.it.util._
//import com.wavesplatform.it.{CanSetScript, MatcherSuiteBase}
//import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
//import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
//import play.api.libs.json.Json
//
//import scala.concurrent.duration._
//
//class OrderTypeTestSuite extends MatcherSuiteBase with CanSetScript {
//  private val aliceAsset =
//    node
//      .broadcastIssue(alice, "AliceCoinOrders", "AliceCoin for tests with order types", someAssetAmount, 0, reissuable = false, smartIssueFee, None)
//      .id
//
//  Seq(aliceAsset, wavesNode1Api.broadcast(IssueUsdTx).id).map(wavesNode1Api.waitForTransaction(_))
//
//  private val predefAssetPair = wavesUsdPair
//  private val aliceWavesPair  = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Waves)
//
//  "Order types verification with SmartContracts" - {
//    val sco1 = s"""
//                 |{-# STDLIB_VERSION 2 #-}
//                 |match tx {
//                 | case o : Order =>
//                 |   o.orderType == Buy
//                 | case s : SetScriptTransaction => true
//                 | case other => throw()
//                 | }
//                 |""".stripMargin
//
//    val sco2 = s"""
//              |{-# STDLIB_VERSION 2 #-}
//              |match tx {
//              | case o : Order =>
//              |    o.orderType == Sell
//              |  case s : SetScriptTransaction => true
//              |  case _ => throw()
//              | }
//      """.stripMargin
//
//    val sco3 = s"""
//                 |{-# STDLIB_VERSION 2 #-}
//                 |match tx {
//                 |  case o : Order =>
//                 |        o.orderType == Buy || o.orderType == Sell
//                 |  case s : SetScriptTransaction => true
//                 |  case _ => throw()
//                 | }
//      """.stripMargin
//
//    "scenarios of order placement" - {
//      "set contracts with only BUY type and then place order" in {
//        setContract(Some(sco1), alice)
//
//        val aliceOrd1 = node
//          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//          .message
//          .id
//        dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Accepted)
//
//        assertBadRequest(
//          node
//            .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//            .message
//            .id)
//
//        dex1Api.cancel(alice, predefAssetPair, aliceOrd1).status should be("OrderCanceled")
//
//        setContract(None, alice)
//      }
//
//      "set contracts with only SELL type and then place order" in {
//        setContract(Some(sco2), alice)
//
//        assertBadRequest(
//          node
//            .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//            .message
//            .id)
//
//        val aliceOrd2 = node
//          .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//          .message
//          .id
//        dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Accepted)
//
//        dex1Api.cancel(alice, aliceWavesPair, aliceOrd2).status should be("OrderCanceled")
//
//        setContract(None, alice)
//      }
//
//      "set contracts with both SELL/BUY types and then place order" in {
//        setContract(Some(sco3), alice)
//
//        val aliceOrd1 = node
//          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//          .message
//          .id
//        dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Accepted)
//
//        val aliceOrd2 = node
//          .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//          .message
//          .id
//        dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Accepted)
//
//        dex1Api.cancel(alice, predefAssetPair, aliceOrd1).status should be("OrderCanceled")
//        dex1Api.cancel(alice, aliceWavesPair, aliceOrd2).status should be("OrderCanceled")
//
//        setContract(None, alice)
//      }
//
//      "place order and then set contract on BUY type" in {
//        val aliceOrd1 = node
//          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//          .message
//          .id
//        dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Accepted)
//
//        val aliceOrd2 = node
//          .placeOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
//          .message
//          .id
//        dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Accepted)
//
//        setContract(Some(sco1), alice)
//
//        val bobOrd1 = node
//          .placeOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
//          .message
//          .id
//        val bobOrd2 = node
//          .placeOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
//          .message
//          .id
//
//        dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Filled)
//        dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Filled)
//        dex1Api.waitForOrderStatus(bobOrd1, OrderStatus.Filled)
//        dex1Api.waitForOrderStatus(bobOrd2, OrderStatus.Filled)
//
//        waitForOrderAtNode(bobOrd1)
//
//        val txs = node.waitTransactionsByOrder(bobOrd2, 1)
//        node.expectSignedBroadcastRejected(Json.toJson(txs.head)) shouldBe TransactionNotAllowedByAccountScript.ErrorCode
//      }
//    }
//  }
//}
