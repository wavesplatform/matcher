package com.wavesplatform.dex.api.grpc

import cats.syntax.either._
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.api.grpc.AssetDescriptionResponse.MaybeDescription
import com.wavesplatform.dex.model.BriefAssetDescription
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.{Asset, Proofs}

object ToVanillaConversions {
  implicit class PbSignedExchangeTransactionOps(val self: SignedExchangeTransaction) extends AnyVal {
    def toVanilla: Either[ValidationError, exchange.ExchangeTransaction] =
      for {
        tx <- self.transaction.fold[Either[ValidationError, ExchangeTransaction]](GenericError("The transaction must be specified").asLeft)(_.asRight)
        data <- tx.data.exchange
          .fold[Either[ValidationError, ExchangeTransactionData]](GenericError("The transaction's data must be specified").asLeft)(_.asRight)
        fee <- tx.fee.toRight(GenericError("The fee must be specified"))
        r <- {
          val proofs = Proofs(self.proofs.map(_.toVanilla))
          tx.version match {
            case 1 =>
              exchange.ExchangeTransactionV1.create(
                data.orders.head.toVanilla.asInstanceOf[exchange.OrderV1], // todo
                data.orders.last.toVanilla.asInstanceOf[exchange.OrderV1],
                data.amount,
                data.price,
                data.buyMatcherFee,
                data.sellMatcherFee,
                fee.amount,
                tx.timestamp,
                proofs.toSignature
              )
            case 2 =>
              exchange.ExchangeTransactionV2.create(
                data.orders.head.toVanilla,
                data.orders.last.toVanilla,
                data.amount,
                data.price,
                data.buyMatcherFee,
                data.sellMatcherFee,
                fee.amount,
                tx.timestamp,
                proofs
              )
            case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v") // ??
          }
        }
      } yield r
  }

  implicit class PbOrderOps(val order: Order) extends AnyVal {
    def toVanilla: exchange.Order =
      exchange.Order(
        PublicKey(order.senderPublicKey.toVanilla),
        PublicKey(order.matcherPublicKey.toVanilla),
        exchange.AssetPair(Asset.fromProtoId(order.getAssetPair.getAmountAssetId), Asset.fromProtoId(order.getAssetPair.getPriceAssetId)),
        order.orderSide match {
          case Order.Side.BUY             => exchange.OrderType.BUY
          case Order.Side.SELL            => exchange.OrderType.SELL
          case Order.Side.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.getMatcherFee.amount,
        order.proofs.map(_.toByteArray: ByteStr),
        order.version.toByte,
        IssuedAsset(order.matcherFeeAssetId.toVanilla)
      )
  }

  implicit class PbByteStringOps(val self: ByteString) extends AnyVal {
    def toVanilla: ByteStr = ByteStr(self.toByteArray)
  }

  implicit class PbMaybeDescriptionOps(val self: MaybeDescription) extends AnyVal {
    def toVanilla: Option[BriefAssetDescription] = self match {
      case MaybeDescription.Empty => None
      case MaybeDescription.Description(value) =>
        Some(
          BriefAssetDescription(
            name = value.name.toVanilla,
            decimals = value.decimals,
            hasScript = value.hasScript
          ))
    }
  }
}
