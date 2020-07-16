package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.order.{AssetPair, Order}
import com.wavesplatform.protobuf.transaction.ExchangeTransactionData
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.{exchange => ve}
import com.wavesplatform.{account => va}

object WavesToPbConversions {

  implicit class VanillaExchangeTransactionOps(tx: ve.ExchangeTransaction) {
    def toPB: SignedExchangeTransaction =
      SignedExchangeTransaction(
        transaction = Some(
          ExchangeTransaction(
            chainId = tx.chainId.toInt,
            senderPublicKey = tx.sender.toPB,
            fee = Some(Amount(assetId = tx.assetFee._1.toPB, amount = tx.assetFee._2)),
            timestamp = tx.timestamp,
            version = tx.version,
            data = ExchangeTransaction.Data.Exchange(
              ExchangeTransactionData(
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

  implicit class VanillaAssetOps(self: Asset) {
    def toPB: ByteString = self match {
      case Asset.IssuedAsset(assetId) => assetId.toPB
      case Asset.Waves                => ByteString.EMPTY
    }
  }

  implicit class VanillaAddressOps(self: Address) {
    def toPB: ByteString = ByteStr(self.bytes).toPB
  }

  implicit class VanillaOrderOps(order: ve.Order) {
    def toPB: Order =
      Order(
        chainId = va.AddressScheme.current.chainId.toInt,
        senderPublicKey = order.senderPublicKey.toPB,
        matcherPublicKey = order.matcherPublicKey.toPB,
        assetPair = Some(AssetPair(order.assetPair.amountAsset.toPB, order.assetPair.priceAsset.toPB)),
        orderSide = order.orderType match {
          case ve.OrderType.BUY  => Order.Side.BUY
          case ve.OrderType.SELL => Order.Side.SELL
        },
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = Some(Amount(order.matcherFeeAssetId.toPB, order.matcherFee)),
        version = order.version,
        proofs = order.proofs.map(_.toPB)
      )
  }

  implicit class VanillaByteStrOps(val self: ByteStr) extends AnyVal {
    def toPB: ByteString = ByteString.copyFrom(self.arr)
  }
}
