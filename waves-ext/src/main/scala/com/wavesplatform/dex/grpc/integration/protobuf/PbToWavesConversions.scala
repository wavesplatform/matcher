package com.wavesplatform.dex.grpc.integration.protobuf

import java.nio.charset.StandardCharsets

import cats.syntax.either._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.services.AssetDescriptionResponse.MaybeDescription
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.order.Order
import com.wavesplatform.protobuf.transaction.ExchangeTransactionData
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.{Asset, Proofs}

object PbToWavesConversions {

  implicit class PbSignedExchangeTransactionOps(val self: SignedExchangeTransaction) extends AnyVal {
    def toVanilla: Either[ValidationError, exchange.ExchangeTransaction] =
      for {
        tx <- self.transaction.fold[Either[ValidationError, ExchangeTransaction]](GenericError("The transaction must be specified").asLeft)(_.asRight)
        data <- tx.data.exchange
          .fold[Either[ValidationError, ExchangeTransactionData]](GenericError("The transaction's data must be specified").asLeft)(_.asRight)
        fee <- tx.fee.fold[Either[ValidationError, Amount]](GenericError("The transaction's fee must be specified").asLeft)(_.asRight)
        r <- {
          val proofs = Proofs(self.proofs.map(_.toVanilla))
          tx.version match {
            case 1 | 2 =>
              exchange.ExchangeTransaction.create(
                version = tx.version.toByte,
                order1 = data.orders.head.toVanilla,
                order2 = data.orders.last.toVanilla,
                amount = data.amount,
                price = data.price,
                buyMatcherFee = data.buyMatcherFee,
                sellMatcherFee = data.sellMatcherFee,
                fee = fee.amount,
                timestamp = tx.timestamp,
                proofs = proofs,
                chainId = AddressScheme.current.chainId
              )
            case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
          }
        }
      } yield r
  }

  implicit class PbOrderOps(val order: Order) extends AnyVal {
    def toVanilla: exchange.Order =
      exchange.Order(
        senderPublicKey = PublicKey(order.senderPublicKey.toVanilla),
        matcherPublicKey = PublicKey(order.matcherPublicKey.toVanilla),
        assetPair = exchange.AssetPair(order.getAssetPair.amountAssetId.toVanillaAsset, order.getAssetPair.priceAssetId.toVanillaAsset),
        orderType = order.orderSide match {
          case Order.Side.BUY             => exchange.OrderType.BUY
          case Order.Side.SELL            => exchange.OrderType.SELL
          case Order.Side.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = order.matcherFee.map(_.amount) match {
          case None    => throw new IllegalArgumentException("The matcherFee must be specified")
          case Some(x) => x
        },
        proofs = order.proofs.map(_.toVanilla),
        version = order.version.toByte,
        matcherFeeAssetId = order.matcherFee.map(_.assetId) match {
          case None        => throw new IllegalArgumentException("The matcherFeeAssetId must be specified")
          case Some(asset) => asset.toVanillaAsset
        }
      )
  }

  implicit class PbByteStringOps(val self: ByteString) extends AnyVal {
    def toVanilla: ByteStr        = ByteStr(self.toByteArray)
    def toVanillaAsset: Asset     = if (self.isEmpty) Asset.Waves else Asset.IssuedAsset(self.toVanilla)
    def toVanillaAddress: Address = Address.fromBytes { self.toByteArray } explicitGetErr ()
  }

  implicit class PbMaybeDescriptionOps(val self: MaybeDescription) extends AnyVal {
    def toVanilla: Option[BriefAssetDescription] = self match {
      case MaybeDescription.Empty => None
      case MaybeDescription.Description(value) =>
        Some(
          BriefAssetDescription(
            name = value.name.toString(StandardCharsets.UTF_8),
            decimals = value.decimals,
            hasScript = value.hasScript
          )
        )
    }
  }
}
