package com.wavesplatform.dex.queue

import cats.syntax.either._
import cats.syntax.option._
import com.google.protobuf.empty.{Empty => PbEmpty}
import com.google.protobuf.{ByteString => PbByteString}
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.domain.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions.PbByteStringOps
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder}
import kamon.context.Context
// format: off
import com.wavesplatform.dex.protobuf.order.{
  I64,
  AssetPair => PbAssetPair,
  CancelOrder => PbCancelOrder,
  Order => PbOrder,
  PlaceLimitOrder => PbPlaceLimitOrder,
  PlaceMarketOrder => PbPlaceMarketOrder,
  ValidatedCommand => PbValidatedCommand
}
// format: on
import com.wavesplatform.dex.tool.KamonTraceUtils

object ValidatedCommandPbConversions {

  def toPb(validatedCommand: ValidatedCommand): PbValidatedCommand = {
    def writeCtx(maybeCtx: Option[Context]): PbByteString =
      maybeCtx.map(ctx => PbByteString.copyFrom(KamonTraceUtils.writeCtx(ctx))).getOrElse(PbByteString.EMPTY)

    validatedCommand match {
      case ValidatedCommand.PlaceOrder(lo, maybeCtx) =>
        val pbAssetPair = writeToPbAssetPair(lo.order.assetPair)
        val cmd = PbValidatedCommand.Command.PlaceLimitOrder(
          PbPlaceLimitOrder(writeToPbOrder(lo.order).some, lo.percentMinFee.map(I64(_)), lo.percentConstMinFee.map(I64(_)))
        )
        PbValidatedCommand(pbAssetPair.some, writeCtx(maybeCtx), cmd)

      case ValidatedCommand.PlaceMarketOrder(mo, maybeCtx) =>
        val pbAssetPair = writeToPbAssetPair(mo.order.assetPair)
        val cmd = PbValidatedCommand.Command.PlaceMarketOrder(
          PbPlaceMarketOrder(writeToPbOrder(mo.order).some, mo.availableForSpending, mo.percentMinFee.map(I64(_)), mo.percentConstMinFee.map(I64(_)))
        )
        PbValidatedCommand(pbAssetPair.some, writeCtx(maybeCtx), cmd)

      case ValidatedCommand.CancelOrder(assetPair, orderId, source, owner, maybeCtx) =>
        val pbAssetPair = writeToPbAssetPair(assetPair)
        val pbSource = source match {
          case Source.NotTracked => PbCancelOrder.Source.NOT_TRACKED
          case Source.Request => PbCancelOrder.Source.REQUEST
          case Source.Expiration => PbCancelOrder.Source.EXPIRATION
          case Source.BalanceTracking => PbCancelOrder.Source.BALANCE_TRACKING
        }
        val pbOwner = owner.map(x => PbByteString.copyFrom(x.bytes)).getOrElse(PbByteString.EMPTY)
        val cmd = PbValidatedCommand.Command.CancelOrder(
          PbCancelOrder(PbByteString.copyFrom(orderId), pbSource, pbOwner)
        )
        PbValidatedCommand(pbAssetPair.some, writeCtx(maybeCtx), cmd)

      case ValidatedCommand.CancelAllOrders(assetPair, maybeCtx) =>
        val pbAssetPair = writeToPbAssetPair(assetPair)
        val cmd = PbValidatedCommand.Command.CancelAllOrders(PbEmpty())
        PbValidatedCommand(pbAssetPair.some, writeCtx(maybeCtx), cmd)

      case ValidatedCommand.DeleteOrderBook(assetPair, maybeCtx) =>
        val pbAssetPair = writeToPbAssetPair(assetPair)
        val cmd = PbValidatedCommand.Command.DeleteOrderBook(PbEmpty())
        PbValidatedCommand(pbAssetPair.some, writeCtx(maybeCtx), cmd)
    }
  }

  def fromPb(pbValidatedCommand: PbValidatedCommand): Either[ValidationError, ValidatedCommand] =
    pbValidatedCommand.assetPair.toRight(GenericError("Asset pair is empty")).map(readPbAssetPair).flatMap { assetPair =>
      val maybeCtx = Either.catchNonFatal(KamonTraceUtils.readCtx(pbValidatedCommand.kamonCtx.toVanilla)).toOption

      val pbCmd = pbValidatedCommand.command
      if (pbCmd.isPlaceLimitOrder)
        for {
          pbPlaceLimitOrder <- pbCmd.placeLimitOrder.toRight(GenericError("Place limit order is empty"))
          pbOrder <- pbPlaceLimitOrder.order.toRight(GenericError("Order is empty"))
          order <- readPbOrder(pbOrder)
        } yield ValidatedCommand.PlaceOrder(
          limitOrder = LimitOrder(
            o = order,
            percentMinFee = pbPlaceLimitOrder.percentMinFee.map(_.value),
            percentConstMinFee = pbPlaceLimitOrder.percentConstMinFee.map(_.value)
          ),
          maybeCtx = maybeCtx
        )
      else if (pbCmd.isPlaceMarketOrder)
        for {
          pbPlaceMarketOrder <- pbCmd.placeMarketOrder.toRight(GenericError("Place market order is empty"))
          pbOrder <- pbPlaceMarketOrder.order.toRight(GenericError("Order is empty"))
          order <- readPbOrder(pbOrder)
        } yield ValidatedCommand.PlaceMarketOrder(
          marketOrder = MarketOrder(
            o = order,
            availableForSpending = pbPlaceMarketOrder.availableForSpending,
            percentMinFee = pbPlaceMarketOrder.percentMinFee.map(_.value),
            percentConstMinFee = pbPlaceMarketOrder.percentConstMinFee.map(_.value)
          ),
          maybeCtx = maybeCtx
        )
      else if (pbCmd.isCancelOrder)
        for {
          pbCancelOrder <- pbCmd.cancelOrder.toRight(GenericError("Cancel order is empty"))
          orderId = pbCancelOrder.orderId.toByteArray
          source <- pbCancelOrder.source match {
            case PbCancelOrder.Source.NOT_TRACKED => Right(Source.NotTracked)
            case PbCancelOrder.Source.REQUEST => Right(Source.Request)
            case PbCancelOrder.Source.EXPIRATION => Right(Source.Expiration)
            case PbCancelOrder.Source.BALANCE_TRACKING => Right(Source.BalanceTracking)
            case PbCancelOrder.Source.Unrecognized(v) => Left(GenericError(s"Unknown source type: $v"))
          }
          owner <- {
            if (pbCancelOrder.owner.isEmpty)
              Right(Option.empty)
            else
              Address.fromBytes(pbCancelOrder.owner.toVanilla).map(_.some)
          }
        } yield ValidatedCommand.CancelOrder(assetPair, orderId, source, owner, maybeCtx)
      else if (pbCmd.isDeleteOrderBook)
        Right(ValidatedCommand.DeleteOrderBook(assetPair, maybeCtx))
      else
        Right(ValidatedCommand.CancelAllOrders(assetPair, maybeCtx))
    }

  private def writeToPbOrder(order: Order): PbOrder =
    PbOrder(
      chainId = AddressScheme.current.chainId.toInt,
      senderPublicKey = PbByteString.copyFrom(order.senderPublicKey),
      matcherPublicKey = PbByteString.copyFrom(order.matcherPublicKey),
      assetPair = writeToPbAssetPair(order.assetPair).some,
      orderSide =
        order.orderType match {
          case OrderType.BUY => PbOrder.Side.BUY
          case OrderType.SELL => PbOrder.Side.SELL
        },
      amount = order.amount,
      price = order.price,
      timestamp = order.timestamp,
      expiration = order.expiration,
      feeAssetId = writeToPbAsset(order.feeAsset),
      matcherFee = order.matcherFee,
      version = order.version,
      proofs = order.proofs.map(PbByteString.copyFrom(_))
    )

  private def readPbOrder(pbOrder: PbOrder): Either[ValidationError, Order] =
    for {
      pbAssetPair <- pbOrder.assetPair.toRight(GenericError("Asset pair is empty"))
      assetPair = readPbAssetPair(pbAssetPair)
      orderType <- pbOrder.orderSide match {
        case PbOrder.Side.BUY => Right(OrderType.BUY)
        case PbOrder.Side.SELL => Right(OrderType.SELL)
        case PbOrder.Side.Unrecognized(v) => Left(GenericError(s"Unknown order type: $v"))
      }
    } yield Order(
      senderPublicKey = PublicKey(pbOrder.senderPublicKey.toVanilla),
      matcherPublicKey = PublicKey(pbOrder.matcherPublicKey.toVanilla),
      assetPair = assetPair,
      orderType = orderType,
      amount = pbOrder.amount,
      price = pbOrder.price,
      timestamp = pbOrder.timestamp,
      expiration = pbOrder.expiration,
      feeAsset = pbOrder.feeAssetId.toVanillaAsset,
      matcherFee = pbOrder.matcherFee,
      proofs = pbOrder.proofs.map(_.toVanilla),
      version = pbOrder.version.toByte
    )

  private def writeToPbAssetPair(assetPair: AssetPair): PbAssetPair =
    PbAssetPair(
      writeToPbAsset(assetPair.amountAsset),
      writeToPbAsset(assetPair.priceAsset)
    )

  private def readPbAssetPair(pbAssetPair: PbAssetPair): AssetPair =
    AssetPair(pbAssetPair.amountAssetId.toVanillaAsset, pbAssetPair.priceAssetId.toVanillaAsset)

  private def writeToPbAsset(asset: Asset): PbByteString =
    asset.compatId.map(PbByteString.copyFrom(_)).getOrElse(PbByteString.EMPTY)

}
