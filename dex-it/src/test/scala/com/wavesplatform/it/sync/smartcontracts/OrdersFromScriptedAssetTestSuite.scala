package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.dex.it.waves.MkWavesEntities
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.wavesj.transactions.IssueTransaction

/**
  * Rules:
  * 1. If the script fails during placing, the matcher must reject an order
  * 2. If the script fails during execution, the matcher must cancel both orders (submitted and counter)
  */
class OrdersFromScriptedAssetTestSuite extends MatcherSuiteBase {

  import OrdersFromScriptedAssetTestSuite._

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(s"""waves.dex.price-assets = ["${allowAsset.id}", "${denyAsset.id}", "${unscriptedAsset.id}"]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(issueUnscriptedAssetTx, issueAllowAssetTx, issueAllowAsset2Tx, issueAllowAsset3Tx, issueDenyAssetTx)
    dex1.start()
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    dex1.api.cancelAll(matcher)
  }

  "can place if the script returns TRUE" in {
    val pair    = AssetPair(unscriptedAsset, allowAsset)
    val counter = mkOrder(matcher, pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = smartTradeFee)
    placeAndAwaitAtDex(counter)
    dex1.api.cancel(matcher, counter)
  }

  "can't place if the script returns FALSE" in {
    val pair  = AssetPair(unscriptedAsset, denyAsset)
    val order = mkOrder(matcher, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, matcherFee = smartTradeFee, version = 2)
    dex1.api.tryPlace(order) should failWith(
      11536130, // AssetScriptDeniedOrder
      MatcherError.Params(assetId = Some(denyAsset.id.toString))
    )
  }

  "can execute against unscripted, if the script returns TRUE" in {
    val pair = AssetPair(unscriptedAsset, allowAsset)

    info("place counter")
    placeAndAwaitAtDex(mkOrder(matcher, pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = smartTradeFee))

    info("place a submitted order")
    placeAndAwaitAtDex(mkOrder(matcher, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = smartTradeFee), Status.Filled)
  }

  "can execute against scripted, if both scripts returns TRUE" in {
    val allowAsset100 = mkAllow(100)
    broadcastAndAwait(allowAsset100)

    val pair = AssetPair(IssuedAsset(allowAsset100.getId), allowAsset)

    info("place a counter order")
    val counter = mkOrder(matcher, pair, OrderType.SELL, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = twoSmartTradeFee)
    placeAndAwaitAtDex(counter)

    info("place a submitted order")
    val submitted = mkOrder(matcher, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = twoSmartTradeFee)
    dex1.api.place(submitted)

    info("both orders are cancelled")
    dex1.api.waitForOrderStatus(submitted, Status.Filled)
    dex1.api.waitForOrderStatus(counter, Status.Filled)
  }

  "can't execute against unscripted, if the script returns FALSE" in {
    info("place a counter order")
    val pair    = AssetPair(allowAsset2, unscriptedAsset)
    val counter = mkOrder(matcher, pair, OrderType.SELL, 100001, 2 * Order.PriceConstant, version = 2, matcherFee = smartTradeFee)
    placeAndAwaitAtDex(counter)

    info("update a script")
    val setAssetScript = mkSetAssetScript(matcher, allowAsset2, DenyBigAmountScript)
    broadcastAndAwait(setAssetScript)

    info("a counter order wasn't rejected")
    dex1.api.orderStatus(counter).status shouldBe Status.Accepted

    info("place a submitted order")
    val submitted = mkOrder(matcher, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = smartTradeFee)
    dex1.api.place(submitted)

    info("two orders form an invalid transaction")
    dex1.api.waitForOrderStatus(submitted, Status.Filled)
    dex1.api.waitForOrderStatus(counter, Status.PartiallyFilled)

    val txs = dex1.api.waitForTransactionsByOrder(submitted, 1)
    val r   = wavesNode1.api.tryBroadcast(txs.head)
    r shouldBe Symbol("left")
    r.left.get.error shouldBe 308 // node's ApiError TransactionNotAllowedByAssetScript.Id
  }

  "can't execute against scripted, if one script returns FALSE" in {
    info("place a counter order")
    val pair    = AssetPair(allowAsset3, allowAsset)
    val counter = mkOrder(matcher, pair, OrderType.SELL, 100001, 2 * Order.PriceConstant, version = 2, matcherFee = twoSmartTradeFee)
    placeAndAwaitAtDex(counter)

    info("update a script")
    val setAssetScriptTx = mkSetAssetScript(matcher, allowAsset3, DenyBigAmountScript)
    broadcastAndAwait(setAssetScriptTx)

    info("a counter order wasn't rejected")
    dex1.api.orderStatus(counter).status shouldBe Status.Accepted

    info("place a submitted order")
    val submitted = mkOrder(matcher, pair, OrderType.BUY, 100000, 2 * Order.PriceConstant, version = 2, matcherFee = twoSmartTradeFee)
    dex1.api.place(submitted)

    info("two orders form an invalid transaction")
    dex1.api.waitForOrderStatus(submitted, Status.Filled)
    dex1.api.waitForOrderStatus(counter, Status.PartiallyFilled)

    val txs = dex1.api.waitForTransactionsByOrder(submitted, 1)
    val r   = wavesNode1.api.tryBroadcast(txs.head)
    r shouldBe Symbol("left")
    r.left.get.error shouldBe 308 // node's ApiError TransactionNotAllowedByAssetScript.Id
  }
}

object OrdersFromScriptedAssetTestSuite {

  import MkWavesEntities.mkIssue
  import com.wavesplatform.dex.it.config.PredefinedAccounts.matcher
  import com.wavesplatform.dex.it.waves.Implicits.toVanilla
  import com.wavesplatform.dex.waves.WavesFeeConstants.smartIssueFee

  private def mkAllow(id: Int): IssueTransaction =
    mkIssue(matcher, s"AllowAsset-$id", Int.MaxValue / 3, 0, smartIssueFee, Some(Scripts.alwaysTrue))

  private val issueUnscriptedAssetTx = mkIssue(matcher, "UnscriptedAsset", Int.MaxValue / 3, 0)
  private val unscriptedAsset        = IssuedAsset(issueUnscriptedAssetTx.getId)

  private val issueAllowAssetTx = mkAllow(0)
  private val allowAsset        = IssuedAsset(issueAllowAssetTx.getId)

  private val issueAllowAsset2Tx = mkAllow(1)
  private val allowAsset2        = IssuedAsset(issueAllowAsset2Tx.getId)

  private val issueAllowAsset3Tx = mkAllow(2)
  private val allowAsset3        = IssuedAsset(issueAllowAsset3Tx.getId)

  private val issueDenyAssetTx = mkIssue(matcher, "DenyAsset", Int.MaxValue / 3, 0, smartIssueFee, Some(Scripts.alwaysFalse))
  private val denyAsset        = IssuedAsset(issueDenyAssetTx.getId)

  /*
  {-# STDLIB_VERSION 2 #-}
  match tx {
   case tx: ExchangeTransaction => tx.sellOrder.amount <= 100000
   case other => true
  }
   */
  private val DenyBigAmountScript = Scripts.fromBase64(
    "AgQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAE0V4Y2hhbmdlVHJhbnNhY3Rpb24EAAAAAnR4BQAAAAckbWF0Y2gwCQA" +
      "AZwAAAAIAAAAAAAABhqAICAUAAAACdHgAAAAJc2VsbE9yZGVyAAAABmFtb3VudAQAAAAFb3RoZXIFAAAAByRtYXRjaDAGTQhceA=="
  )
}
