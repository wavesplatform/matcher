package com.wavesplatform.dex.it.waves

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.account.{Address, AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.it.waves.WavesFeeConstants._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV2, SetAssetScriptTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV2, LeaseTransaction, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction, TransferTransactionV2}

import scala.concurrent.duration.{Duration, DurationInt}

trait MkWavesEntities {

  def orderVersion: Byte = { ThreadLocalRandom.current.nextInt(3) + 1 }.toByte

  /**
    * @param matcherFeeAssetId If specified IssuedAsset, the version will be automatically set to 3
    */
  def mkOrder(owner: KeyPair,
              pair: AssetPair,
              orderType: OrderType,
              amount: Long,
              price: Long,
              matcherFee: Long = matcherFee,
              matcherFeeAssetId: Asset = Waves,
              ts: Long = System.currentTimeMillis(),
              ttl: Duration = 30.days - 1.seconds,
              version: Byte = orderVersion,
              matcher: PublicKey): Order =
    if (matcherFeeAssetId == Waves)
      Order(
        sender = owner,
        matcher = matcher,
        pair = pair,
        orderType = orderType,
        amount = amount,
        price = price,
        timestamp = ts,
        expiration = ts + ttl.toMillis,
        matcherFee = matcherFee,
        version = version,
      )
    else
      Order(
        sender = owner,
        matcher = matcher,
        pair = pair,
        orderType = orderType,
        amount = amount,
        price = price,
        timestamp = ts,
        expiration = ts + ttl.toMillis,
        matcherFee = matcherFee,
        version = 3,
        matcherFeeAssetId = matcherFeeAssetId
      )

  def mkTransfer(sender: KeyPair,
                 recipient: Address,
                 amount: Long,
                 asset: Asset,
                 feeAmount: Long = minFee,
                 feeAsset: Asset = Waves,
                 timestamp: Long = System.currentTimeMillis()): TransferTransaction =
    TransferTransactionV2
      .selfSigned(
        assetId = asset,
        sender = sender,
        recipient = recipient,
        amount = amount,
        timestamp = timestamp,
        feeAssetId = feeAsset,
        feeAmount = feeAmount,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

  def mkMassTransfer(sender: KeyPair,
                     asset: Asset,
                     transfers: List[ParsedTransfer],
                     fee: Long = setScriptFee,
                     ts: Long = System.currentTimeMillis()): MassTransferTransaction =
    MassTransferTransaction
      .selfSigned(
        sender = sender,
        assetId = asset,
        transfers = transfers,
        timestamp = ts,
        feeAmount = fee,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

  def mkLease(sender: KeyPair,
              recipient: Address,
              amount: Long,
              fee: Long = leasingFee,
              timestamp: Long = System.currentTimeMillis()): LeaseTransaction =
    LeaseTransactionV2
      .selfSigned(
        sender = sender,
        amount = amount,
        fee = fee,
        timestamp = timestamp,
        recipient = recipient
      )
      .explicitGet()

  def mkLeaseCancel(sender: KeyPair, leaseId: ByteStr, fee: Long = leasingFee, timestamp: Long = System.currentTimeMillis()): LeaseCancelTransaction =
    LeaseCancelTransactionV2
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = sender,
        leaseId = leaseId,
        fee = fee,
        timestamp = timestamp
      )
      .explicitGet()

  def mkIssue(issuer: KeyPair,
              name: String,
              quantity: Long,
              decimals: Int = 8,
              fee: Long = issueFee,
              script: Option[Script] = None,
              reissuable: Boolean = false,
              timestamp: Long = System.currentTimeMillis()): IssueTransaction =
    IssueTransactionV2
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = issuer,
        name = name.getBytes(),
        description = s"$name asset".getBytes(StandardCharsets.UTF_8),
        quantity = quantity,
        decimals = decimals.toByte,
        reissuable = false,
        script = script,
        fee = fee,
        timestamp = timestamp
      )
      .explicitGet()

  def mkSetAccountScript(accountOwner: KeyPair,
                         script: Option[Script],
                         fee: Long = setScriptFee,
                         ts: Long = System.currentTimeMillis()): SetScriptTransaction =
    SetScriptTransaction
      .selfSigned(
        sender = accountOwner,
        script = script,
        fee = fee,
        timestamp = ts
      )
      .explicitGet()

  def mkSetAccountScriptText(accountOwner: KeyPair,
                             scriptText: Option[String],
                             fee: Long = setScriptFee,
                             ts: Long = System.currentTimeMillis()): SetScriptTransaction = {
    val script = scriptText.map { x =>
      ScriptCompiler.compile(x.stripMargin, ScriptEstimatorV2).explicitGet()._1
    }

    mkSetAccountScript(accountOwner, script, fee, ts)
  }

  def mkSetAssetScript(assetOwner: KeyPair,
                       asset: IssuedAsset,
                       script: Option[Script],
                       fee: Long = setAssetScriptFee,
                       ts: Long = System.currentTimeMillis()): SetAssetScriptTransaction =
    SetAssetScriptTransaction
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = assetOwner,
        asset = asset,
        script = script,
        fee = fee,
        timestamp = ts
      )
      .explicitGet()

  def mkSetAssetScriptText(assetOwner: KeyPair,
                           asset: IssuedAsset,
                           scriptText: String,
                           fee: Long = setAssetScriptFee,
                           ts: Long = System.currentTimeMillis()): SetAssetScriptTransaction = {
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    mkSetAssetScript(assetOwner, asset, Some(script), fee, ts)
  }

  def mkExchange(buyOrderOwner: KeyPair,
                 sellOrderOwner: KeyPair,
                 pair: AssetPair,
                 amount: Long,
                 price: Long,
                 matcherFee: Long = matcherFee,
                 ts: Long = System.currentTimeMillis(),
                 matcher: KeyPair): ExchangeTransaction = {
    val buyOrder  = mkOrder(buyOrderOwner, pair, OrderType.BUY, amount, price, matcherFee, matcher = matcher)
    val sellOrder = mkOrder(sellOrderOwner, pair, OrderType.SELL, amount, price, matcherFee, matcher = matcher)
    ExchangeTransactionV2
      .create(
        matcher = matcher,
        buyOrder = buyOrder,
        sellOrder = sellOrder,
        amount = amount,
        price = price,
        buyMatcherFee = buyOrder.matcherFee,
        sellMatcherFee = sellOrder.matcherFee,
        fee = matcherFee,
        timestamp = ts
      )
      .explicitGet()
  }
}

object MkWavesEntities extends MkWavesEntities
