package com.wavesplatform.dex.it.waves

import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.it.config.GenesisConfig
import im.mak.waves.transactions.account.{Address => JAddress, PrivateKey => JPrivateKey, PublicKey => JPublicKey}
import im.mak.waves.transactions.common.{Amount, AssetId, Id, Proof => JProof}
import im.mak.waves.transactions.exchange.{Order => JOrder, OrderType => JOrderType}
import im.mak.waves.transactions.{ExchangeTransaction => JExchangeTransaction}

trait ToWavesJConversions {

  implicit def wavesJAssetIdToVanillaByteStr(x: AssetId): ByteStr = ByteStr(x.bytes())
  implicit def wavesJIdToVanillaByteStr(x: Id): ByteStr = ByteStr(x.bytes())

  implicit def byteStrToWavesJId(x: ByteStr): Id = Id.as(x.arr)

  implicit def privateKeyToWavesJ(x: KeyPair): JPrivateKey = JPrivateKey.as(x.privateKey.arr)
  implicit def publicKeyToWavesJ(x: PublicKey): JPublicKey = JPublicKey.as(x.arr)

  implicit def addressToWavesJ(x: Address): JAddress = JAddress.as(x.bytes.arr)

  implicit def assetIdToWavesJ(x: Asset): AssetId = x match {
    case Asset.IssuedAsset(id) => AssetId.as(id.arr)
    case Asset.Waves => AssetId.WAVES
  }

  implicit def orderTypeToWavesJ(x: OrderType): JOrderType = if (x == OrderType.BUY) JOrderType.BUY else JOrderType.SELL

  implicit class OrderOps(val self: Order) {

    def toWavesJ(chainId: Byte = GenesisConfig.chainId): JOrder = {
      val unsignedOrder =
        JOrder
          .builder(
            self.orderType,
            Amount.of(self.amount, self.assetPair.amountAsset),
            Amount.of(self.price, self.assetPair.priceAsset),
            self.matcherPublicKey
          )
          .chainId(chainId)
          .sender(self.sender)
          .timestamp(self.timestamp)
          .expiration(self.expiration)
          .fee(Amount.of(self.matcherFee, self.feeAsset))
          .version(self.version)
          .getUnsigned

      self.proofs.zipWithIndex.foldLeft(unsignedOrder) {
        case (resultOrder, (proof, index)) =>
          resultOrder.setProof(index, JProof.as(proof.arr))
      }
    }

  }

  implicit class ExchangeTransactionOps(val self: ExchangeTransaction) {

    def toWavesJ(chainId: Byte = GenesisConfig.chainId): JExchangeTransaction = {
      val unsignedTx =
        JExchangeTransaction
          .builder(
            self.buyOrder.toWavesJ(chainId),
            self.sellOrder.toWavesJ(chainId),
            self.amount,
            self.price,
            self.buyMatcherFee,
            self.sellMatcherFee
          )
          .chainId(chainId)
          .sender(self.sender)
          .fee(self.fee)
          .timestamp(self.timestamp)
          .version(2)
          .getUnsigned

      self.proofs.zipWithIndex.foldLeft(unsignedTx) {
        case (resultTx, (proof, idx)) =>
          resultTx.setProof(idx, JProof.as(proof.arr))
      }
    }

  }

}

object Implicits extends ToWavesJConversions
