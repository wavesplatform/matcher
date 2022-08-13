package com.wavesplatform.dex.grpc.integration.protobuf

import cats.syntax.either._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.services.AssetDescriptionResponse.MaybeDescription
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.order.Order
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.{Asset, Proofs, TxExchangeAmount, TxMatcherFee, TxOrderPrice}

import java.nio.charset.StandardCharsets

object PbToWavesConversions {

  implicit class PbSignedExchangeTransactionOps(val self: SignedExchangeTransaction) extends AnyVal {

    def toVanilla: Either[ValidationError, exchange.ExchangeTransaction] =
      for {
        tx <- self.transaction.toRight(GenericError("The transaction must be specified"))
        data <- tx.data.exchange.toRight(GenericError("The transaction's data must be specified"))
        fee <- tx.fee.toRight(GenericError("The transaction's fee must be specified"))
        o1 <- Either.catchNonFatal(data.orders.head.toVanilla)
          .leftMap(th => GenericError(s"Transaction's orders is corrupted: ${th.getMessage}"))
        o2 <- Either.catchNonFatal(data.orders.last.toVanilla)
          .leftMap(th => GenericError(s"Transaction's orders is corrupted: ${th.getMessage}"))
        r <- {
          val proofs = Proofs(self.proofs.map(_.toVanilla))
          tx.version match {
            case 1 | 2 | 3 =>
              exchange.ExchangeTransaction.create(
                version = tx.version.toByte,
                order1 = o1,
                order2 = o2,
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
        orderAuthentication = order.sender match {
          case Order.Sender.SenderPublicKey(key) =>
            exchange.OrderAuthentication.OrderProofs(
              PublicKey(key.toVanilla),
              order.proofs.map(_.toVanilla)
            )
          case Order.Sender.Eip712Signature(sig) =>
            exchange.OrderAuthentication.Eip712Signature(sig.toVanilla)
          case Order.Sender.Empty =>
            throw new IllegalArgumentException("Order should have either senderPublicKey or eip712Signature")
        },
        matcherPublicKey = PublicKey(order.matcherPublicKey.toVanilla),
        assetPair = exchange.AssetPair(order.getAssetPair.amountAssetId.toVanillaAsset, order.getAssetPair.priceAssetId.toVanillaAsset),
        orderType = order.orderSide match {
          case Order.Side.BUY => exchange.OrderType.BUY
          case Order.Side.SELL => exchange.OrderType.SELL
          case Order.Side.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        amount = TxExchangeAmount.from(order.amount)
          .fold(s => throw new IllegalArgumentException(s"Invalid order amount ${order.amount}, error: $s"), identity),
        price = TxOrderPrice.from(order.price)
          .fold(s => throw new IllegalArgumentException(s"Invalid order price ${order.price}, error: $s"), identity),
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = order.matcherFee.map(_.amount) match {
          case None => throw new IllegalArgumentException("The matcherFee must be specified")
          case Some(x) => TxMatcherFee.from(x).fold(
              s => throw new IllegalArgumentException(s"Invalid order fee $x, error: $s"),
              identity
            )
        },
        priceMode = order.priceMode match {
          case Order.PriceMode.ASSET_DECIMALS => exchange.OrderPriceMode.AssetDecimals
          case Order.PriceMode.FIXED_DECIMALS => exchange.OrderPriceMode.FixedDecimals
          case Order.PriceMode.DEFAULT => exchange.OrderPriceMode.Default
          case Order.PriceMode.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order price mode: $v")
        },
        version = order.version.toByte,
        matcherFeeAssetId = order.matcherFee.map(_.assetId) match {
          case None => throw new IllegalArgumentException("The matcherFeeAssetId must be specified")
          case Some(asset) => asset.toVanillaAsset
        }
      )

  }

  implicit class PbByteStringOps(val self: ByteString) extends AnyVal {
    def toVanilla: ByteStr = ByteStr(self.toByteArray)
    def toVanillaAsset: Asset = if (self.isEmpty) Asset.Waves else Asset.IssuedAsset(self.toVanilla)
    def toVanillaAddress: Address = Address.fromBytes(self.toByteArray).explicitGetErr()
  }

  implicit class PbMaybeDescriptionOps(val self: MaybeDescription) extends AnyVal {

    def toVanilla: Option[BriefAssetDescription] = self match {
      case MaybeDescription.Empty => None
      case MaybeDescription.Description(value) =>
        Some(
          BriefAssetDescription(
            name = value.name.toString(StandardCharsets.UTF_8),
            decimals = value.decimals,
            hasScript = value.hasScript,
            isNft = value.nft
          )
        )
    }

  }

}
