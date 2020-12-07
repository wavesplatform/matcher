package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.protobuf.{UnsafeByteOperations, ByteString => PbByteString}
import com.wavesplatform.dex.domain.account.{Address => VAddress, AddressScheme => VAddressScheme}
import com.wavesplatform.dex.domain.asset.{Asset => VAsset}
import com.wavesplatform.dex.domain.bytes.{ByteStr => VByteStr}
import com.wavesplatform.dex.domain.order.{Order => VOrder, OrderType => VOrderType}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction => VExchangeTransaction}
import com.wavesplatform.dex.grpc.integration.services.{ExchangeTransaction, SignedExchangeTransaction}
import com.wavesplatform.protobuf.order.{AssetPair => PbAssetPair, Order => PbOrder}
import com.wavesplatform.protobuf.transaction.{ExchangeTransactionData => PbExchangeTransactionData}
import com.wavesplatform.protobuf.{Amount => PbAmount}

object DexToPbConversions {

  implicit class VanillaExchangeTransactionOps(tx: VExchangeTransaction) {

    def toPB: SignedExchangeTransaction =
      SignedExchangeTransaction(
        transaction = Some(
          ExchangeTransaction(
            chainId = tx.chainByte.getOrElse(VAddressScheme.current.chainId).toInt,
            senderPublicKey = tx.sender.toPB,
            fee = Some(PbAmount(assetId = tx.assetFee._1.toPB, amount = tx.assetFee._2)),
            timestamp = tx.timestamp,
            version = tx.version,
            data = ExchangeTransaction.Data.Exchange(
              PbExchangeTransactionData(
                amount = tx.amount,
                price = tx.price,
                buyMatcherFee = tx.buyMatcherFee,
                sellMatcherFee = tx.sellMatcherFee,
                orders = Seq(tx.buyOrder.toPB, tx.sellOrder.toPB)
              )
            )
          )
        ),
        proofs = tx.proofs.proofs.map(_.toPB)
      )

  }

  implicit class VanillaAssetOps(self: VAsset) {
    def toPB: PbByteString = self.fold(PbByteString.EMPTY)(_.id.toPB)
  }

  implicit class VanillaAddressOps(self: VAddress) {
    def toPB: PbByteString = self.bytes.toPB
  }

  implicit class VanillaOrderOps(order: VOrder) {

    def toPB: PbOrder =
      PbOrder(
        chainId = VAddressScheme.current.chainId.toInt,
        senderPublicKey = order.senderPublicKey.toPB,
        matcherPublicKey = order.matcherPublicKey.toPB,
        assetPair = Some(PbAssetPair(order.assetPair.amountAsset.toPB, order.assetPair.priceAsset.toPB)),
        orderSide = order.orderType match {
          case VOrderType.BUY => PbOrder.Side.BUY
          case VOrderType.SELL => PbOrder.Side.SELL
        },
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = Some(PbAmount(order.feeAsset.toPB, order.matcherFee)),
        version = order.version,
        proofs = order.proofs.map(_.toPB)
      )

  }

  implicit class VanillaByteStrOps(val self: VByteStr) extends AnyVal {
    def toPB: PbByteString = UnsafeByteOperations.unsafeWrap(self) // PbByteString.copyFrom(self.arr)
  }

}
