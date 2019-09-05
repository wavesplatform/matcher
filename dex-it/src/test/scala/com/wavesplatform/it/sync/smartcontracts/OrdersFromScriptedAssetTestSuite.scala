//package com.wavesplatform.it.sync.smartcontracts
//
//import com.typesafe.config.{Config, ConfigFactory}
//import com.wavesplatform.account.AddressScheme
//import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAssetScript
//import com.wavesplatform.common.utils.EitherExt2
//import com.wavesplatform.features.BlockchainFeatures
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.SyncHttpApi.NodeExtSync
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.it.sync.createSignedIssueRequest
//import com.wavesplatform.it.util._
//import com.wavesplatform.lang.script.Script
//import com.wavesplatform.lang.script.v1.ExprScript
//import com.wavesplatform.lang.v1.compiler.Terms
//import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
//import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
//import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
//import com.wavesplatform.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
//import com.wavesplatform.transaction.smart.script.ScriptCompiler
//import play.api.libs.json.Json
//
//import scala.concurrent.duration.DurationInt
//import scala.util.Random
//
///**
//  * Rules:
//  * 1. If the script fails during placing, the matcher must reject an order
//  * 2. If the script fails during execution, the matcher must cancel both orders (submitted and counter)
//  */
//class OrdersFromScriptedAssetTestSuite extends MatcherSuiteBase {
//
//  import OrdersFromScriptedAssetTestSuite._
//
//  override protected def nodeConfigs: Seq[Config] = Configs.map(commonConfig.withFallback)
//
//  "can match orders when SmartAccTrading is still not activated" in {
//    val pair = AssetPair(Waves, IssuedAsset(AllowAsset.id()))
//
//    val counter =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 1, fee = smartTradeFee))
//    dex1Api.waitForOrderStatus(counter.message.id, OrderStatus.Accepted)
//
//    val submitted =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 1, fee = smartTradeFee))
//    dex1Api.waitForOrderStatus(submitted.message.id, OrderStatus.Filled)
//
//    waitForOrderAtNode(submitted.message.id)
//    node.waitForHeight(activationHeight + 1, 2.minutes)
//  }
//
//  "can place if the script returns TRUE" in {
//    val pair = AssetPair.createAssetPair(UnscriptedAssetId, AllowAssetId).get
//    val counterId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee)).message.id
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.Accepted)
//    node.cancelOrder(matcher, pair, counterId)
//  }
//
//  "can't place if the script returns FALSE" in {
//    val pair = AssetPair.createAssetPair(UnscriptedAssetId, DenyAssetId).get
//    val order = Order.buy(
//      matcher,
//      matcher,
//      pair,
//      100000,
//      2 * Order.PriceConstant,
//      timestamp = System.currentTimeMillis(),
//      expiration = System.currentTimeMillis() + 60 * 60 * 30,
//      matcherFee = smartTradeFee,
//      version = 2
//    )
//    node.expectIncorrectOrderPlacement(order, 400, "OrderRejected")
//  }
//
//  "can execute against unscripted, if the script returns TRUE" in {
//    info("place counter")
//    val pair = AssetPair.createAssetPair(UnscriptedAssetId, AllowAssetId).get
//    val counterId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee)).message.id
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.Accepted)
//
//    info("place a submitted order")
//    val submittedId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee)).message.id
//    dex1Api.waitForOrderStatus(submittedId, OrderStatus.Filled)
//  }
//
//  "can execute against scripted, if both scripts returns TRUE" in {
//    val allowAsset2Id = issueAsset()
//
//    info("place a counter order")
//    val pair = AssetPair.createAssetPair(allowAsset2Id, AllowAssetId).get
//    val counterId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee)).message.id
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.Accepted)
//
//    info("place a submitted order")
//    val submittedId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee)).message.id
//
//    info("both orders are cancelled")
//    dex1Api.waitForOrderStatus(submittedId, OrderStatus.Filled)
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.Filled)
//  }
//
//  "can't execute against unscripted, if the script returns FALSE" in {
//    info("place a counter order")
//    val pair = AssetPair.createAssetPair(AllowAsset2Id, UnscriptedAssetId).get
//    val counterId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.SELL, 100001, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee)).message.id
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.Accepted)
//
//    info("update a script")
//    val setAssetScriptId = node.setAssetScript(AllowAsset2Id, matcher.toAddress.toString, 1.waves, Some(DenyBigAmountScript.bytes().base64)).id
//    wavesNode1Api.waitForTransaction(setAssetScriptId)
//
//    info("a counter order wasn't rejected")
//    dex1Api.orderStatus(counterId).status shouldBe "Accepted"
//
//    info("place a submitted order")
//    val submittedId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = smartTradeFee)).message.id
//
//    info("two orders form an invalid transaction")
//    dex1Api.waitForOrderStatus(submittedId, OrderStatus.Filled)
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.PartiallyFilled)
//
//    val txs = node.waitTransactionsByOrder(submittedId, 1)
//    node.expectSignedBroadcastRejected(Json.toJson(txs.head)) shouldBe TransactionNotAllowedByAssetScript.ErrorCode
//  }
//
//  "can't execute against scripted, if one script returns FALSE" in {
//    info("place a counter order")
//    val pair = AssetPair.createAssetPair(AllowAsset3Id, AllowAssetId).get
//    val counterId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.SELL, 100001, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee)).message.id
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.Accepted)
//
//    info("update a script")
//    val setAssetScriptId = node.setAssetScript(AllowAsset3Id, matcher.toAddress.toString, 1.waves, Some(DenyBigAmountScript.bytes().base64)).id
//    wavesNode1Api.waitForTransaction(setAssetScriptId)
//
//    info("a counter order wasn't rejected")
//    dex1Api.orderStatus(counterId).status shouldBe "Accepted"
//
//    info("place a submitted order")
//    val submittedId =
//      dex1Api.place(mkOrder(matcher,pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, fee = twoSmartTradeFee)).message.id
//
//    info("two orders form an invalid transaction")
//    dex1Api.waitForOrderStatus(submittedId, OrderStatus.Filled)
//    dex1Api.waitForOrderStatus(counterId, OrderStatus.PartiallyFilled)
//
//    val txs = node.waitTransactionsByOrder(submittedId, 1)
//    node.expectSignedBroadcastRejected(Json.toJson(txs.head)) shouldBe TransactionNotAllowedByAssetScript.ErrorCode
//  }
//
//  private def issueAsset(): String = {
//    info("issue an asset")
//    val allowAsset2   = mkAllowAsset()
//    val allowAsset2Id = allowAsset2.id().toString
//    node.signedIssue(createSignedIssueRequest(allowAsset2))
//    wavesNode1Api.waitForTransaction(allowAsset2Id)
//    allowAsset2Id
//  }
//
//  override protected def beforeAll(): Unit = {
//    super.beforeAll()
//    val xs = Seq(UnscriptedAsset, AllowAsset, AllowAsset2, AllowAsset3, DenyAsset).map(_.json()).map(wavesNode1Api.broadcast(_))
//    xs.foreach(tx => wavesNode1Api.waitForTransaction(tx.id))
//  }
//}
//
//object OrdersFromScriptedAssetTestSuite {
//
//  private val UnscriptedAsset = IssueTransactionV1
//    .selfSigned(
//      sender = matcher,
//      name = "UnscriptedAsset".getBytes(),
//      description = "unscripted".getBytes(),
//      quantity = Int.MaxValue / 3,
//      decimals = 0,
//      reissuable = false,
//      fee = 1.waves,
//      timestamp = System.currentTimeMillis()
//    )
//    .explicitGet()
//
//  private val UnscriptedAssetId = UnscriptedAsset.id().toString
//
//  private def mkAllowAsset(id: Int = Random.nextInt(1000) + 1): IssueTransactionV2 = {
//    IssueTransactionV2
//      .selfSigned(
//        AddressScheme.current.chainId,
//        sender = matcher,
//        name = s"AllowAsset-$id".getBytes(),
//        description = s"AllowAsset-$id".getBytes(),
//        quantity = Int.MaxValue / 3,
//        decimals = 0,
//        reissuable = false,
//        script = Some(ExprScript(Terms.TRUE).explicitGet()),
//        fee = 1.waves,
//        timestamp = System.currentTimeMillis()
//      )
//      .explicitGet()
//  }
//
//  private val AllowAsset    = mkAllowAsset(0)
//  private val AllowAssetId  = AllowAsset.id().toString
//  private val AllowAsset2   = mkAllowAsset(1)
//  private val AllowAsset2Id = AllowAsset2.id().toString
//  private val AllowAsset3   = mkAllowAsset(1)
//  private val AllowAsset3Id = AllowAsset3.id().toString
//
//  private val DenyAsset = IssueTransactionV2
//    .selfSigned(
//      AddressScheme.current.chainId,
//      sender = matcher,
//      name = "DenyAsset".getBytes(),
//      description = "DenyAsset".getBytes(),
//      quantity = Int.MaxValue / 3,
//      decimals = 0,
//      reissuable = false,
//      script = Some(ExprScript(Terms.FALSE).explicitGet()),
//      fee = 1.waves,
//      timestamp = System.currentTimeMillis()
//    )
//    .explicitGet()
//
//  private val DenyAssetId = DenyAsset.id().toString
//
//  private val DenyBigAmountScript: Script = {
//    val scriptText = s"""
//                        |{-# STDLIB_VERSION 2 #-}
//                        |match tx {
//                        | case tx: ExchangeTransaction => tx.sellOrder.amount <= 100000
//                        | case other => true
//                        |}
//                        |""".stripMargin
//    ScriptCompiler(scriptText, isAssetScript = true, ScriptEstimatorV2).explicitGet()._1
//  }
//
//  val activationHeight = 10
//
//  private val commonConfig = ConfigFactory.parseString(s"""waves {
//                                                          |  blockchain.custom.functionality.pre-activated-features = {
//                                                          |    ${BlockchainFeatures.SmartAssets.id} = 0,
//                                                          |    ${BlockchainFeatures.SmartAccountTrading.id} = $activationHeight
//                                                          |  }
//                                                          |  dex.price-assets = ["$AllowAssetId", "$DenyAssetId", "$UnscriptedAssetId"]
//                                                          |}""".stripMargin)
//}
