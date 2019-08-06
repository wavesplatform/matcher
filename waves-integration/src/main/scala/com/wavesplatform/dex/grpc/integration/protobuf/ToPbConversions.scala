package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.protobuf.transaction.Recipient.Recipient.Address
import com.wavesplatform.protobuf.transaction.{Amount, AssetId}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.{exchange => ve}
import com.wavesplatform.{account => va}

object ToPbConversions {
  implicit class VanillaExchangeTransactionOps(tx: ve.ExchangeTransaction) {
    def toPB: SignedExchangeTransaction =
      SignedExchangeTransaction(
        transaction = Some(
          ExchangeTransaction(
            chainId = tx.chainByte.getOrElse(va.AddressScheme.current.chainId).toInt,
            senderPublicKey = ByteString.copyFrom(tx.sender.bytes.arr),
            fee = Some(Amount(assetId = None, amount = tx.fee)),
            timestamp = tx.timestamp,
            version = tx.version,
            data = ExchangeTransaction.Data.Exchange(
              ExchangeTransactionData(
                amount = tx.amount,
                price = tx.price,
                buyMatcherFee = tx.buyMatcherFee,
                sellMatcherFee = tx.sellMatcherFee,
                orders = Seq(tx.buyOrder.toPB, tx.sellOrder.toPB)
              ))
          )),
        proofs = tx.proofs.proofs.map(_.toPB)
      )
  }

  implicit class VanillaAssetOps(self: Asset) {
    def toPB: AssetId = self match {
      case Asset.IssuedAsset(assetId) => AssetId(AssetId.Asset.IssuedAsset(assetId.toPB))
      case Asset.Waves                => AssetId(AssetId.Asset.Waves(com.google.protobuf.empty.Empty.defaultInstance))
    }
  }

  implicit class VanillaOrderOps(order: ve.Order) {
    def toPB: Order = Order(
      chainId = 0,
      ByteString.copyFrom(order.senderPublicKey),
      ByteString.copyFrom(order.matcherPublicKey),
      Some(Order.AssetPair(Some(order.assetPair.amountAsset.protoId), Some(order.assetPair.priceAsset.protoId))),
      order.orderType match {
        case ve.OrderType.BUY  => Order.Side.BUY
        case ve.OrderType.SELL => Order.Side.SELL
      },
      order.amount,
      order.price,
      order.timestamp,
      order.expiration,
      Some(Amount(Some(order.matcherFeeAssetId.toPB), order.matcherFee)),
      order.version,
      order.proofs.map(_.toPB)
    )
  }

  implicit class VanillaByteStrOps(val self: ByteStr) extends AnyVal {
    def toPB: ByteString = ByteString.copyFrom(self.arr)
  }

  implicit class VanillaAddressOps(val self: va.Address) extends AnyVal {
    def toPB: Address = Address(self.bytes.toPB)
  }
}
