package com.wavesplatform.dex.it.waves

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.dex.domain.account.{Address, AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.model.Normalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.it.config.PredefinedAccounts.matcher
import com.wavesplatform.dex.it.waves.Implicits._
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.waves.WavesFeeConstants._
import com.wavesplatform.wavesj.transactions.{ExchangeTransaction => JExchangeTransaction, _}
import com.wavesplatform.wavesj.{Transactions, Transfer}

import scala.concurrent.duration.{Duration, DurationInt}
import scala.jdk.CollectionConverters._

// TODO Rename
trait MkWavesEntities {

  private val emptyAttachments: java.lang.String = null

  private def orderVersion: Byte = { ThreadLocalRandom.current.nextInt(3) + 1 }.toByte

  def mkKeyPair(seed: String): KeyPair = KeyPair(crypto secureHash seed.getBytes(StandardCharsets.UTF_8))

  /**
    * @param feeAsset If specified IssuedAsset, the version will be automatically set to 3
    * TODO make ttl random by default to solve issue of creating multiple orders in a loop
    */
  def mkOrder(owner: KeyPair,
              pair: AssetPair,
              orderType: OrderType,
              amount: Long,
              price: Long,
              matcherFee: Long = matcherFee,
              feeAsset: Asset = Waves,
              ts: Long = System.currentTimeMillis,
              ttl: Duration = 30.days - 1.seconds,
              version: Byte = orderVersion,
              matcher: PublicKey = matcher): Order =
    if (feeAsset == Waves)
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
        feeAsset = feeAsset
      )

  /**
    * Creates order with denormalized price.
    * For not predefined assets it is required to enrich `PredefinedAssets.assetsDecimalsMap` (use overriding), otherwise 8 decimals will be picked
    */
  def mkOrderDP(owner: KeyPair,
                pair: AssetPair,
                orderType: OrderType,
                amount: Long,
                price: Double,
                matcherFee: Long = matcherFee,
                feeAsset: Asset = Waves,
                ts: Long = System.currentTimeMillis,
                ttl: Duration = 30.days - 1.seconds,
                version: Byte = orderVersion,
                matcher: PublicKey = matcher)(implicit assetDecimalsMap: Map[Asset, Int]): Order = {
    val normalizedPrice = Normalization.normalizePrice(price, assetDecimalsMap(pair.amountAsset), assetDecimalsMap(pair.priceAsset))
    mkOrder(owner, pair, orderType, amount, normalizedPrice, matcherFee, feeAsset, ts, ttl, version, matcher)
  }

  def mkTransfer(sender: KeyPair,
                 recipient: Address,
                 amount: Long,
                 asset: Asset,
                 feeAmount: Long = minFee,
                 feeAsset: Asset = Waves,
                 timestamp: Long = System.currentTimeMillis): TransferTransaction = {
    Transactions.makeTransferTx(sender, recipient, amount, asset, feeAmount, feeAsset, emptyAttachments, timestamp)
  }

  def mkMassTransfer(sender: KeyPair,
                     asset: Asset,
                     transfers: List[Transfer],
                     fee: Long = massTransferDefaultFee,
                     timestamps: Long = System.currentTimeMillis): MassTransferTransaction = {
    Transactions.makeMassTransferTx(sender, asset, transfers.asJava, fee, emptyAttachments, timestamps)
  }

  def mkLease(sender: KeyPair,
              recipient: Address,
              amount: Long,
              fee: Long = leasingFee,
              timestamp: Long = System.currentTimeMillis): LeaseTransaction = {
    Transactions.makeLeaseTx(sender, recipient, amount, fee, timestamp)
  }

  def mkLeaseCancel(sender: KeyPair, leaseId: ByteStr, fee: Long = leasingFee, timestamp: Long = System.currentTimeMillis): LeaseCancelTransaction = {
    Transactions.makeLeaseCancelTx(sender, AddressScheme.current.chainId, leaseId.base58, fee, timestamp)
  }

  def mkIssue(issuer: KeyPair,
              name: String,
              quantity: Long,
              decimals: Int = 8,
              fee: Long = issueFee,
              script: Option[ByteStr] = None,
              reissuable: Boolean = false,
              timestamp: Long = System.currentTimeMillis): IssueTransaction = {
    Transactions.makeIssueTx(issuer,
                             AddressScheme.current.chainId,
                             name,
                             s"$name asset",
                             quantity,
                             decimals.toByte,
                             reissuable,
                             script.map(_.base64).orNull,
                             fee,
                             timestamp)
  }

  def mkIssueExtended(issuer: KeyPair,
                      name: String,
                      quantity: Long,
                      decimals: Int = 8,
                      fee: Long = issueFee,
                      script: Option[ByteStr] = None,
                      reissuable: Boolean = false,
                      timestamp: Long = System.currentTimeMillis): IssueResults = {

    val tx          = mkIssue(issuer, name, quantity, decimals, fee, script, reissuable, timestamp)
    val assetId     = toVanilla(tx.getId)
    val issuedAsset = IssuedAsset(assetId)

    IssueResults(tx, assetId, issuedAsset)
  }

  def mkSetAccountScript(accountOwner: KeyPair, script: ByteStr): SetScriptTransaction =
    mkSetAccountScript(accountOwner, Some(script))

  def mkResetAccountScript(accountOwner: KeyPair, fee: Long = setScriptFee, timestamp: Long = System.currentTimeMillis): SetScriptTransaction =
    mkSetAccountScript(accountOwner, None, fee, timestamp)

  def mkSetAccountScript(accountOwner: KeyPair,
                         script: Option[ByteStr],
                         fee: Long = setScriptFee,
                         timestamp: Long = System.currentTimeMillis): SetScriptTransaction = {
    Transactions.makeScriptTx(accountOwner, script.map(_.base64).orNull, AddressScheme.current.chainId, fee, timestamp)
  }

  def mkSetAssetScript(assetOwner: KeyPair,
                       asset: IssuedAsset,
                       script: ByteStr,
                       fee: Long = setAssetScriptFee,
                       timestamp: Long = System.currentTimeMillis): SetAssetScriptTransaction = {
    Transactions.makeSetAssetScriptTransaction(assetOwner, AddressScheme.current.chainId, asset, script.base64, fee, timestamp)
  }

  def mkExchange(buyOrderOwner: KeyPair,
                 sellOrderOwner: KeyPair,
                 pair: AssetPair,
                 amount: Long,
                 price: Long,
                 matcherFee: Long = matcherFee,
                 timestamp: Long = System.currentTimeMillis,
                 matcher: KeyPair): JExchangeTransaction =
    toWavesJ(mkDomainExchange(buyOrderOwner, sellOrderOwner, pair, amount, price, matcherFee, ts = timestamp, matcher = matcher))

  def mkDomainExchange(buyOrderOwner: KeyPair,
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

  def mkBurn(sender: KeyPair, asset: Asset, amount: Long, fee: Long = burnFee, ts: Long = System.currentTimeMillis): BurnTransaction = {
    Transactions.makeBurnTx(sender, AddressScheme.current.chainId, asset, amount, burnFee, ts)
  }
}

object MkWavesEntities extends MkWavesEntities {
  case class IssueResults(tx: IssueTransaction, assetId: ByteStr, asset: IssuedAsset)
}
