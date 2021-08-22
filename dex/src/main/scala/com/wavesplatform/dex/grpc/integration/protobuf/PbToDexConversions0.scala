package com.wavesplatform.dex.grpc.integration.protobuf

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.order.{OrderType => DexOrderType, Order => DexOrder}
import com.wavesplatform.protobuf.order.{Order => PbOrder}
import com.wavesplatform.protobuf.transaction.{ExchangeTransactionData, SignedTransaction, Transaction}

//'0' suffix in order to prevent naming overlapping
object PbToDexConversions0 {

  implicit final class PbSignedExchangeTransactionOps(val signedTx: SignedTransaction) extends AnyVal {

    def getOrdersVanilla: Either[ValidationError, Seq[DexOrder]] =
      for {
        tx <-
          signedTx.transaction
            .fold[Either[ValidationError, Transaction]](GenericError("The transaction must be specified").asLeft)(_.asRight)
        data <- tx.data.exchange
          .fold[Either[ValidationError, ExchangeTransactionData]](GenericError("The transaction's data must be specified").asLeft)(_.asRight)
        orders <- data.orders.toList.traverse(_.toVanilla)
      } yield orders

  }

  implicit final class PbOrderOps(val order: PbOrder) extends AnyVal {

    def toVanilla: Either[ValidationError, DexOrder] =
      for {
        orderType <- order.orderSide match {
          case PbOrder.Side.BUY => Right(DexOrderType.BUY)
          case PbOrder.Side.SELL => Right(DexOrderType.SELL)
          case PbOrder.Side.Unrecognized(v) => Left(GenericError(s"Unknown order type: $v"))
        }
        matcherFee <- {
          order.matcherFee.map(_.amount) match {
            case None => Left(GenericError("The matcherFee must be specified"))
            case Some(x) => Right(x)
          }
        }
        feeAsset <- {
          order.matcherFee.map(_.assetId) match {
            case None => Left(GenericError("The matcherFeeAssetId must be specified"))
            case Some(asset) => Right(asset.toVanillaAsset)
          }
        }
      } yield DexOrder(
        senderPublicKey = PublicKey(order.senderPublicKey.toVanilla),
        matcherPublicKey = PublicKey(order.matcherPublicKey.toVanilla),
        assetPair = AssetPair(order.getAssetPair.amountAssetId.toVanillaAsset, order.getAssetPair.priceAssetId.toVanillaAsset),
        orderType = orderType,
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = matcherFee,
        proofs = order.proofs.map(_.toVanilla),
        version = order.version.toByte,
        feeAsset = feeAsset
      )

  }

  implicit final class PbByteStringOps(val self: ByteString) extends AnyVal {
    def toVanilla: ByteStr = ByteStr(self.toByteArray)
    def toVanillaAsset: Asset = if (self.isEmpty) Asset.Waves else Asset.IssuedAsset(self.toVanilla)
  }

}
