package com.wavesplatform.dex.it.waves

import com.wavesplatform.dex.domain.account.{Address, AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import im.mak.waves.transactions.account.{Address => JAddress, PrivateKey => JPrivateKey, PublicKey => JPublicKey}
import im.mak.waves.transactions.common.{Amount, AssetId, Id, Proof => JProof}
import im.mak.waves.transactions.exchange.{Order => JOrder, OrderType => JOrderType}
import im.mak.waves.transactions.{ExchangeTransaction => JExchangeTransaction}

trait ToWavesJConversions {

  implicit def toVanilla(x: AssetId): ByteStr = ByteStr(x.bytes())
  implicit def toVanilla(x: Id): ByteStr      = ByteStr(x.bytes())

  implicit def toWavesJ(x: ByteStr): Id = Id.as(x.arr)

  implicit def toWavesJ(x: KeyPair): JPrivateKey  = JPrivateKey.as(x.privateKey.arr)
  implicit def toWavesJ(x: PublicKey): JPublicKey = JPublicKey.as(x.arr)

  implicit def toWavesJ(x: Address): JAddress = JAddress.as(x.bytes.arr)

  implicit def toWavesJ(x: Asset): AssetId = x match {
    case Asset.IssuedAsset(id) => AssetId.as(id.arr)
    case Asset.Waves           => AssetId.WAVES
  }

  implicit def toWavesJ(x: OrderType): JOrderType = if (x == OrderType.BUY) JOrderType.BUY else JOrderType.SELL

  implicit def toWavesJ(x: Order): JOrder = {

    val unsignedOrder =
      JOrder
        .builder(x.orderType, Amount.of(x.amount, x.assetPair.amountAsset), Amount.of(x.price, x.assetPair.priceAsset), x.matcherPublicKey)
        .sender(x.sender)
        .timestamp(x.timestamp)
        .expiration(x.expiration)
        .fee(Amount.of(x.matcherFee, x.feeAsset))
        .version(x.version)
        .getUnsigned

    x.proofs.zipWithIndex.foldLeft(unsignedOrder) {
      case (resultOrder, (proof, index)) =>
        resultOrder.setProof(index, JProof.as(proof.arr))
    }
  }

  implicit def toWavesJ(x: ExchangeTransaction): JExchangeTransaction = {
    val unsignedTx =
      JExchangeTransaction
        .builder(x.buyOrder, x.sellOrder, x.amount, x.price, x.buyMatcherFee, x.sellMatcherFee)
        .sender(x.sender)
        .fee(x.fee)
        .timestamp(x.timestamp)
        .chainId(AddressScheme.current.chainId)
        .version(2)
        .getUnsigned

    x.proofs.zipWithIndex.foldLeft(unsignedTx) {
      case (resultTx, (proof, idx)) =>
        resultTx.setProof(idx, JProof.as(proof.arr))
    }
  }
}

object Implicits extends ToWavesJConversions
