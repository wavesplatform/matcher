package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.protobuf.{UnsafeByteOperations, ByteString => PbByteString}
import com.wavesplatform.dex.domain.account.{Address => DexAddress, AddressScheme => DexAddressScheme}
import com.wavesplatform.dex.domain.asset.{Asset => DexAsset}
import com.wavesplatform.dex.domain.bytes.{ByteStr => DexByteStr}
import com.wavesplatform.dex.domain.order.{OrderAuthentication => DexOrderAuthentication, Order => DexOrder, OrderType => DexOrderType}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction => DexExchangeTransaction}
import com.wavesplatform.dex.grpc.integration.services.{ExchangeTransaction => PbDexExchangeTransaction, SignedExchangeTransaction => PbDexSignedExchangeTransaction}
import com.wavesplatform.protobuf.order.{AssetPair => PbAssetPair, Order => PbOrder}
import com.wavesplatform.protobuf.transaction.{ExchangeTransactionData => PbExchangeTransactionData}
import com.wavesplatform.protobuf.{Amount => PbAmount}
import com.wavesplatform.protobuf.transaction.{SignedTransaction => PbSignedTransaction}
import com.wavesplatform.protobuf.transaction.{Transaction => PbTransaction}

object DexToPbConversions {

  implicit final class VanillaExchangeTransactionOps(val tx: DexExchangeTransaction) extends AnyVal {

    def toPB: PbDexSignedExchangeTransaction =
      PbDexSignedExchangeTransaction(
        transaction = Some(
          PbDexExchangeTransaction(
            chainId = tx.chainByte.getOrElse(DexAddressScheme.current.chainId).toInt,
            senderPublicKey = tx.sender.toPB,
            fee = Some(PbAmount(assetId = tx.assetFee._1.toPB, amount = tx.assetFee._2)),
            timestamp = tx.timestamp,
            version = tx.version,
            data = PbDexExchangeTransaction.Data.Exchange(PbExchangeTransactionData(
              amount = tx.amount,
              price = tx.withFixedPriceUnsafe.price,
              buyMatcherFee = tx.buyMatcherFee,
              sellMatcherFee = tx.sellMatcherFee,
              orders = Seq(tx.buyOrder.toPB, tx.sellOrder.toPB)
            ))
          )
        ),
        proofs = tx.proofs.proofs.map(_.toPB)
      )

    def toPBWaves: PbSignedTransaction =
      PbSignedTransaction(
        transaction =
          PbSignedTransaction.Transaction.WavesTransaction(
            PbTransaction(
              chainId = tx.chainByte.getOrElse(DexAddressScheme.current.chainId).toInt,
              senderPublicKey = tx.sender.toPB,
              fee = Some(PbAmount(assetId = tx.assetFee._1.toPB, amount = tx.assetFee._2)),
              timestamp = tx.timestamp,
              version = tx.version,
              data = PbTransaction.Data.Exchange(PbExchangeTransactionData(
                amount = tx.amount,
                price = tx.price,
                buyMatcherFee = tx.buyMatcherFee,
                sellMatcherFee = tx.sellMatcherFee,
                orders = Seq(tx.buyOrder.toPB, tx.sellOrder.toPB)
              ))
            )
          ),
        proofs = tx.proofs.proofs.map(_.toPB)
      )

  }

  implicit final class VanillaAssetOps(val self: DexAsset) extends AnyVal {
    def toPB: PbByteString = self.fold(PbByteString.EMPTY)(_.id.toPB)
  }

  implicit final class VanillaAddressOps(val self: DexAddress) extends AnyVal {
    def toPB: PbByteString = self.bytes.toPB
  }

  implicit final class VanillaOrderOps(val order: DexOrder) extends AnyVal {

    def toPB: PbOrder =
      PbOrder(
        chainId = DexAddressScheme.current.chainId.toInt,
        sender = order.orderAuthentication match {
          case DexOrderAuthentication.OrderProofs(sender, _) => PbOrder.Sender.SenderPublicKey(sender.toPB)
          case DexOrderAuthentication.Eip712Signature(sig) => PbOrder.Sender.Eip712Signature(sig.toPB)
        },
        matcherPublicKey = order.matcherPublicKey.toPB,
        assetPair = Some(PbAssetPair(order.assetPair.amountAsset.toPB, order.assetPair.priceAsset.toPB)),
        orderSide = order.orderType match {
          case DexOrderType.BUY => PbOrder.Side.BUY
          case DexOrderType.SELL => PbOrder.Side.SELL
        },
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = Some(PbAmount(order.feeAsset.toPB, order.matcherFee)),
        version = order.version,
        proofs = order.proofs.map(_.toPB),
        priceMode =
          if (order.version <= 3) PbOrder.PriceMode.DEFAULT else PbOrder.PriceMode.ASSET_DECIMALS
      )

  }

  implicit final class VanillaByteStrOps(val self: DexByteStr) extends AnyVal {
    def toPB: PbByteString = UnsafeByteOperations.unsafeWrap(self) // PbByteString.copyFrom(self.arr)
  }

}
