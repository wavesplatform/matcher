package com.wavesplatform.dex.grpc.integration.protobuf

import cats.instances.list._
import cats.syntax.traverse._
import com.google.protobuf.{ByteString => PbByteString}
import com.wavesplatform.dex.domain.account.{Address => DexAddress, PublicKey => DexPublicKey}
import com.wavesplatform.dex.domain.asset.{Asset => DexAsset, AssetPair => DexAssetPair}
import com.wavesplatform.dex.domain.bytes.{ByteStr => DexByteStr}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.order.{OrderAuthentication, Order => DexOrder, OrderType => DexOrderType}
import com.wavesplatform.dex.domain.utils._
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.services.AssetDescriptionResponse.MaybeDescription
import com.wavesplatform.protobuf.order.{Order => PbOrder}
import com.wavesplatform.protobuf.transaction.{SignedTransaction => PbSignedTransaction}

import java.nio.charset.StandardCharsets

object PbToDexConversions {

  implicit final class PbSignedExchangeTransactionOps(val signedTx: PbSignedTransaction) extends AnyVal {

    def getOrdersVanilla: Either[ValidationError, Seq[DexOrder]] =
      for {
        tx <-
          signedTx.transaction.wavesTransaction.toRight(GenericError("The transaction must be specified"))
        data <- tx.data.exchange.toRight(GenericError("The transaction's data must be specified"))
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
        oa <- order.sender match {
          case PbOrder.Sender.SenderPublicKey(key) =>
            Right(OrderAuthentication.OrderProofs(DexPublicKey(key.toVanilla), order.proofs.map(_.toVanilla)))
          case PbOrder.Sender.Eip712Signature(eip) =>
            Right(OrderAuthentication.Eip712Signature(eip.toVanilla))
          case _ =>
            Left(GenericError("Order authentication must be specified"))
        }
      } yield DexOrder(
        orderAuthentication = oa,
        matcherPublicKey = DexPublicKey(order.matcherPublicKey.toVanilla),
        assetPair = DexAssetPair(order.getAssetPair.amountAssetId.toVanillaAsset, order.getAssetPair.priceAssetId.toVanillaAsset),
        orderType = orderType,
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = matcherFee,
        version = order.version.toByte,
        feeAsset = feeAsset
      )

  }

  implicit class PbByteStringOps(val self: PbByteString) extends AnyVal {
    def toVanilla: DexByteStr = DexByteStr(self.toByteArray)
    def toVanillaAsset: DexAsset = if (self.isEmpty) DexAsset.Waves else DexAsset.IssuedAsset(self.toVanilla)
    def toVanillaAddress: DexAddress = DexAddress.fromBytes(self.toByteArray).explicitGet()
    def toVanillaPublicKey: DexPublicKey = DexPublicKey(self.toByteArray)
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
