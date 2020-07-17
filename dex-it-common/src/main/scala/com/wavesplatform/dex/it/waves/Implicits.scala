package com.wavesplatform.dex.it.waves

import com.wavesplatform.dex.domain.account.{Address, AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.wavesj.matcher.{Order => JOrder, OrderV1 => JOrderV1, OrderV2 => JOrderV2, OrderV3 => JOrderV3}
import com.wavesplatform.wavesj.transactions.{
  ExchangeTransaction => JExchangeTransaction,
  ExchangeTransactionV1 => JExchangeTransactionV1,
  ExchangeTransactionV2 => JExchangeTransactionV2
}
import com.wavesplatform.wavesj.{ByteString, PrivateKeyAccount, PublicKeyAccount, AssetPair => JAssetPair}

import scala.jdk.CollectionConverters._

trait ToWavesJConversions {

  implicit def toVanilla(x: ByteString): ByteStr = ByteStr(x.getBytes)

  implicit def toWavesJ(x: KeyPair): PrivateKeyAccount  = PrivateKeyAccount.fromPrivateKey(x.privateKey.base58, AddressScheme.current.chainId)
  implicit def toWavesJ(x: PublicKey): PublicKeyAccount = new PublicKeyAccount(x.arr, AddressScheme.current.chainId)
  implicit def toWavesJ(x: Address): String             = x.stringRepr
  implicit def toWavesJ(x: Asset): String = x match {
    case Asset.IssuedAsset(id) => id.base58
    case Asset.Waves           => null
  }

  implicit def toWavesJ(x: AssetPair): JAssetPair  = new JAssetPair(x.amountAsset, x.priceAsset)
  implicit def toWavesJ(x: OrderType): JOrder.Type = if (x == OrderType.BUY) JOrder.Type.BUY else JOrder.Type.SELL
  implicit def toWavesJ(x: ByteStr): ByteString    = new ByteString(x.arr)
  implicit def toWavesJ(x: Order): JOrder = x.version match {
    case 1 =>
      new JOrderV1(
        x.sender,
        x.matcherPublicKey,
        x.orderType,
        x.assetPair,
        x.amount,
        x.price,
        x.timestamp,
        x.expiration,
        x.matcherFee,
        new ByteString(x.signature)
      )
    case 2 =>
      new JOrderV2(
        x.sender,
        x.matcherPublicKey,
        x.orderType,
        x.assetPair,
        x.amount,
        x.price,
        x.timestamp,
        x.expiration,
        x.matcherFee,
        x.version,
        x.proofs.proofs.map(toWavesJ).asJava
      )
    case 3 =>
      new JOrderV3(
        x.sender,
        x.matcherPublicKey,
        x.orderType,
        x.assetPair,
        x.amount,
        x.price,
        x.timestamp,
        x.expiration,
        x.matcherFee,
        x.feeAsset,
        x.version,
        x.proofs.proofs.map(toWavesJ).asJava
      )
  }

  implicit def toWavesJ(x: ExchangeTransaction): JExchangeTransaction = x.version match {
    case 1 =>
      new JExchangeTransactionV1(
        x.buyOrder,
        x.sellOrder,
        x.amount,
        x.price,
        x.buyMatcherFee,
        x.sellMatcherFee,
        x.fee,
        x.timestamp,
        x.proofs.toSignature
      )
    case 2 =>
      new JExchangeTransactionV2(
        x.buyOrder,
        x.sellOrder,
        x.amount,
        x.price,
        x.buyMatcherFee,
        x.sellMatcherFee,
        x.fee,
        x.timestamp,
        x.proofs.proofs.map(toWavesJ).asJava
      )
  }
}

object Implicits extends ToWavesJConversions
