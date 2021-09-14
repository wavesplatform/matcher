package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.it.config.PredefinedAccounts.generateNewAccount
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.transactions.{IssueTransaction, Transaction}

import java.nio.charset.StandardCharsets

class ExchangeTransactionMinimumFeeTestSuite extends IntegrationSuiteBase {

  /**
   *      {-# STDLIB_VERSION 5 #-}
   *      {-# CONTENT_TYPE EXPRESSION #-}
   *      {-# SCRIPT_TYPE ACCOUNT #-}
   *
   *      match tx {
   *          case _: ExchangeTransaction | Order => getInteger(tx.sender, "1").valueOrElse(1) > 0
   *          case _ => true
   *      }
   */
  val accountScriptLess200 = "BQQAAAAHJG1hdGNoMAUAAAACdHgDAwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAAAVPc" +
    "mRlcgYJAAABAAAAAgUAAAAHJG1hdGNoMAIAAAATRXhjaGFuZ2VUcmFuc2FjdGlvbgkAAGYAAAACCQEAAAALdmFsdWVPc" +
    "kVsc2UAAAACCQAEGgAAAAIIBQAAAAJ0eAAAAAZzZW5kZXICAAAAATEAAAAAAAAAAAEAAAAAAAAAAAAGpMemwQ=="

  /**
   *      {-# STDLIB_VERSION 4 #-}
   *      {-# CONTENT_TYPE EXPRESSION #-}
   *      {-# SCRIPT_TYPE ACCOUNT #-}
   *
   *      match tx {
   *          case _: ExchangeTransaction | Order =>
   *              getInteger(tx.sender, "1").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "2").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "3").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "4").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "5").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "6").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "7").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "8").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "9").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "0").valueOrElse(1) > 0 &&
   *              getInteger(tx.sender, "a").valueOrElse(1) > 0
   *          case _ => true
   *      }
   */
  val accountScript = "BAQAAAAHJG1hdGNoMAUAAAACdHgDAwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAAAVPcmRlcgYJ" +
    "AAABAAAAAgUAAAAHJG1hdGNoMAIAAAATRXhjaGFuZ2VUcmFuc2FjdGlvbgMDAwMDAwMDAwMJAABmAAAAAgkBAAAAC3" +
    "ZhbHVlT3JFbHNlAAAAAgkABBoAAAACCAUAAAACdHgAAAAGc2VuZGVyAgAAAAExAAAAAAAAAAABAAAAAAAAAAAACQAA" +
    "ZgAAAAIJAQAAAAt2YWx1ZU9yRWxzZQAAAAIJAAQaAAAAAggFAAAAAnR4AAAABnNlbmRlcgIAAAABMgAAAAAAAAAAAQ" +
    "AAAAAAAAAAAAcJAABmAAAAAgkBAAAAC3ZhbHVlT3JFbHNlAAAAAgkABBoAAAACCAUAAAACdHgAAAAGc2VuZGVyAgAA" +
    "AAEzAAAAAAAAAAABAAAAAAAAAAAABwkAAGYAAAACCQEAAAALdmFsdWVPckVsc2UAAAACCQAEGgAAAAIIBQAAAAJ0eA" +
    "AAAAZzZW5kZXICAAAAATQAAAAAAAAAAAEAAAAAAAAAAAAHCQAAZgAAAAIJAQAAAAt2YWx1ZU9yRWxzZQAAAAIJAAQa" +
    "AAAAAggFAAAAAnR4AAAABnNlbmRlcgIAAAABNQAAAAAAAAAAAQAAAAAAAAAAAAcJAABmAAAAAgkBAAAAC3ZhbHVlT3" +
    "JFbHNlAAAAAgkABBoAAAACCAUAAAACdHgAAAAGc2VuZGVyAgAAAAE2AAAAAAAAAAABAAAAAAAAAAAABwkAAGYAAAAC" +
    "CQEAAAALdmFsdWVPckVsc2UAAAACCQAEGgAAAAIIBQAAAAJ0eAAAAAZzZW5kZXICAAAAATcAAAAAAAAAAAEAAAAAAA" +
    "AAAAAHCQAAZgAAAAIJAQAAAAt2YWx1ZU9yRWxzZQAAAAIJAAQaAAAAAggFAAAAAnR4AAAABnNlbmRlcgIAAAABOAAA" +
    "AAAAAAAAAQAAAAAAAAAAAAcJAABmAAAAAgkBAAAAC3ZhbHVlT3JFbHNlAAAAAgkABBoAAAACCAUAAAACdHgAAAAGc2" +
    "VuZGVyAgAAAAE5AAAAAAAAAAABAAAAAAAAAAAABwkAAGYAAAACCQEAAAALdmFsdWVPckVsc2UAAAACCQAEGgAAAAII" +
    "BQAAAAJ0eAAAAAZzZW5kZXICAAAAATAAAAAAAAAAAAEAAAAAAAAAAAAHCQAAZgAAAAIJAQAAAAt2YWx1ZU9yRWxzZQ" +
    "AAAAIJAAQaAAAAAggFAAAAAnR4AAAABnNlbmRlcgIAAAABYQAAAAAAAAAAAQAAAAAAAAAAAAcG1wszkg=="

  /**
   *    {-# STDLIB_VERSION 4 #-}
   *    {-# CONTENT_TYPE EXPRESSION #-}
   *    {-# SCRIPT_TYPE ASSET #-}
   *
   *    match tx {
   *        case t: TransferTransaction =>  true
   *        case _: ExchangeTransaction =>
   *            getInteger(tx.sender, "1").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "2").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "3").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "4").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "5").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "6").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "7").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "8").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "9").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "0").valueOrElse(1) > 0 &&
   *            getInteger(tx.sender, "a").valueOrElse(1) > 0
   *        case _ => true
   *    }
   */
  val assetScript = "BAQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAE1RyYW5zZmVyVH" +
    "JhbnNhY3Rpb24EAAAAAXQFAAAAByRtYXRjaDAGAwkAAAEAAAACBQAAAAckbWF0Y2gwAgAAABNFeGNoYW5nZVRyYW5z" +
    "YWN0aW9uAwMDAwMDAwMDAwkAAGYAAAACCQEAAAALdmFsdWVPckVsc2UAAAACCQAEGgAAAAIIBQAAAAJ0eAAAAAZzZW" +
    "5kZXICAAAAATEAAAAAAAAAAAEAAAAAAAAAAAAJAABmAAAAAgkBAAAAC3ZhbHVlT3JFbHNlAAAAAgkABBoAAAACCAUA" +
    "AAACdHgAAAAGc2VuZGVyAgAAAAEyAAAAAAAAAAABAAAAAAAAAAAABwkAAGYAAAACCQEAAAALdmFsdWVPckVsc2UAAA" +
    "ACCQAEGgAAAAIIBQAAAAJ0eAAAAAZzZW5kZXICAAAAATMAAAAAAAAAAAEAAAAAAAAAAAAHCQAAZgAAAAIJAQAAAAt2" +
    "YWx1ZU9yRWxzZQAAAAIJAAQaAAAAAggFAAAAAnR4AAAABnNlbmRlcgIAAAABNAAAAAAAAAAAAQAAAAAAAAAAAAcJAA" +
    "BmAAAAAgkBAAAAC3ZhbHVlT3JFbHNlAAAAAgkABBoAAAACCAUAAAACdHgAAAAGc2VuZGVyAgAAAAE1AAAAAAAAAAAB" +
    "AAAAAAAAAAAABwkAAGYAAAACCQEAAAALdmFsdWVPckVsc2UAAAACCQAEGgAAAAIIBQAAAAJ0eAAAAAZzZW5kZXICAA" +
    "AAATYAAAAAAAAAAAEAAAAAAAAAAAAHCQAAZgAAAAIJAQAAAAt2YWx1ZU9yRWxzZQAAAAIJAAQaAAAAAggFAAAAAnR4" +
    "AAAABnNlbmRlcgIAAAABNwAAAAAAAAAAAQAAAAAAAAAAAAcJAABmAAAAAgkBAAAAC3ZhbHVlT3JFbHNlAAAAAgkABB" +
    "oAAAACCAUAAAACdHgAAAAGc2VuZGVyAgAAAAE4AAAAAAAAAAABAAAAAAAAAAAABwkAAGYAAAACCQEAAAALdmFsdWVP" +
    "ckVsc2UAAAACCQAEGgAAAAIIBQAAAAJ0eAAAAAZzZW5kZXICAAAAATkAAAAAAAAAAAEAAAAAAAAAAAAHCQAAZgAAAA" +
    "IJAQAAAAt2YWx1ZU9yRWxzZQAAAAIJAAQaAAAAAggFAAAAAnR4AAAABnNlbmRlcgIAAAABMAAAAAAAAAAAAQAAAAAA" +
    "AAAAAAcJAABmAAAAAgkBAAAAC3ZhbHVlT3JFbHNlAAAAAgkABBoAAAACCAUAAAACdHgAAAAGc2VuZGVyAgAAAAFhAA" +
    "AAAAAAAAABAAAAAAAAAAAABwaOhypm"

  val scriptedAccount = generateNewAccount("scripted".getBytes(StandardCharsets.UTF_8), 0)
  val scriptedAccountLess200 = generateNewAccount("scripted".getBytes(StandardCharsets.UTF_8), 1)

  val issueScriptedAmountTx: IssueTransaction =
    mkIssue(alice, "scriptedAmount", defaultAssetQuantity, script = Some(Scripts.fromBase64(assetScript)))

  val issueScriptedPriceTx: IssueTransaction =
    mkIssue(alice, "scriptedPrice", defaultAssetQuantity, script = Some(Scripts.fromBase64(assetScript)))

  val scriptedAmount: IssuedAsset = IssuedAsset(issueScriptedAmountTx.id())
  val scriptedPrice: IssuedAsset = IssuedAsset(issueScriptedPriceTx.id())

  val scriptedAmountWavesPair: AssetPair = AssetPair(scriptedAmount, Waves)
  val scriptedAmountScriptedPricePair: AssetPair = AssetPair(scriptedAmount, scriptedPrice)
  val btcScriptedPricePair: AssetPair = AssetPair(btc, scriptedPrice)

  "Node should set correct fee for Exchange transaction when" - {

    "amount asset is scripted" in broadcastAndValidate(
      mkExchange(alice, alice, scriptedAmountWavesPair, 1.btc, 1.waves, matcher = alice, matcherFee = 0.007.waves),
      0.007.waves
    )

    "price asset is scripted" in broadcastAndValidate(
      mkExchange(alice, alice, btcScriptedPricePair, 10.waves, 1.btc, matcher = alice, matcherFee = 0.007.waves),
      0.007.waves
    )

    "fee asset of sell order is scripted" in broadcastAndValidate(
      mkExchange(alice, alice, wavesUsdPair, 10.waves, 1.usd, matcher = alice, sellOrderFeeAsset = scriptedAmount),
      0.003.waves
    )

    "fee asset of buy order is scripted" in broadcastAndValidate(
      mkExchange(alice, alice, wavesUsdPair, 10.waves, 1.usd, matcher = alice, buyOrderFeeAsset = scriptedPrice),
      0.003.waves
    )

    "sell order owners' account is scripted" in broadcastAndValidate(
      mkExchange(alice, scriptedAccount, wavesUsdPair, 10.waves, 1.usd, matcher = alice, buyOrderVersion = 3.toByte, sellOrderVersion = 3.toByte),
      0.003.waves
    )

    "buy order owners' account is scripted" in broadcastAndValidate(
      mkExchange(scriptedAccount, alice, wavesUsdPair, 10.waves, 1.usd, matcher = alice, buyOrderVersion = 3.toByte, sellOrderVersion = 3.toByte),
      0.003.waves
    )

    "matcher account is scripted (more than 200 complexity)" in broadcastAndValidate(
      mkExchange(alice, bob, wavesUsdPair, 10.waves, 1.usd, matcher = scriptedAccount, matcherFee = 0.007.waves),
      0.007.waves
    )

    "matcher account is scripted (less than 200 complexity)" in broadcastAndValidate(
      mkExchange(alice, bob, wavesUsdPair, 10.waves, 1.usd, matcher = scriptedAccountLess200, matcherFee = 0.003.waves),
      0.003.waves
    )

    "buy and sell orders owners, amount and price assets, matcher and fee assets of both orders are scripted" in broadcastAndValidate(
      mkExchange(
        scriptedAccount,
        scriptedAccount,
        scriptedAmountScriptedPricePair,
        10.waves,
        1.usd,
        matcher = scriptedAccount,
        sellOrderFeeAsset = scriptedAmount,
        buyOrderFeeAsset = scriptedPrice,
        matcherFee = 0.015.waves
      ),
      0.015.waves
    )
  }

  def broadcastAndValidate(tx: Transaction, expectedFee: Long): Unit = {
    broadcastAndAwait(tx)
    wavesNode1.api.transactionInfo(tx.id()).fee().value() shouldBe expectedFee
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx, issueScriptedAmountTx, issueScriptedPriceTx)

    broadcastAndAwait(
      mkTransfer(alice, scriptedAccount, 100.usd, usd),
      mkTransfer(bob, alice, 100.btc, btc),
      mkTransfer(bob, scriptedAccount, 100.btc, btc),
      mkTransfer(alice, scriptedAccountLess200, 1000.waves, Waves),
      mkTransfer(alice, scriptedAccount, 1000.waves, Waves),
      mkTransfer(alice, scriptedAccount, 1000.waves, scriptedAmount, 0.007.waves),
      mkTransfer(alice, scriptedAccount, 1000.waves, scriptedPrice, 0.007.waves)
    )

    broadcastAndAwait(
      mkSetAccountScript(scriptedAccount, Scripts.fromBase64(accountScript)),
      mkSetAccountScript(scriptedAccountLess200, Scripts.fromBase64(accountScriptLess200))
    )

  }

}
