package com.wavesplatform.dex.model

import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.util.Codecs.ByteBufferExt

trait OrderInfo[+S <: OrderStatus] {
  def version: Byte
  def side: OrderType
  def amount: Long
  def price: Long
  def matcherFee: Long
  def feeAsset: Asset
  def timestamp: Long
  def status: S
  def assetPair: AssetPair
  def orderType: AcceptedOrderType
  def avgWeighedPrice: Long
}

object OrderInfo {
  type FinalOrderInfo = OrderInfo[OrderStatus.Final]

  def v1[S <: OrderStatus](side: OrderType, amount: Long, price: Long, timestamp: Long, status: S, assetPair: AssetPair): OrderInfo[S] =
    Impl(1, side, amount, price, 300000L, Waves, timestamp, status, assetPair, AcceptedOrderType.Limit, price)

  def v2[S <: OrderStatus](order: Order, status: S): OrderInfo[S] =
    v2(order.orderType, order.amount, order.price, order.matcherFee, order.feeAsset, order.timestamp, status, order.assetPair)

  def v2[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair): OrderInfo[S] =
    Impl(2, side, amount, price, matcherFee, matcherFeeAssetId, timestamp, status, assetPair, AcceptedOrderType.Limit, price)

  def v3[S <: OrderStatus](ao: AcceptedOrder, status: S): OrderInfo[S] = {
    import ao.order
    val acceptedOrderType = if (ao.isLimit) AcceptedOrderType.Limit else AcceptedOrderType.Market
    v3(order.orderType, order.amount, order.price, order.matcherFee, order.feeAsset, order.timestamp, status, order.assetPair, acceptedOrderType)
  }

  def v3[S <: OrderStatus](order: Order, status: S, orderType: AcceptedOrderType): OrderInfo[S] =
    v3(order.orderType, order.amount, order.price, order.matcherFee, order.feeAsset, order.timestamp, status, order.assetPair, orderType)

  def v3[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair,
                           orderType: AcceptedOrderType): OrderInfo[S] =
    Impl(3, side, amount, price, matcherFee, matcherFeeAssetId, timestamp, status, assetPair, orderType, price)

  def v4[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair,
                           orderType: AcceptedOrderType,
                           avgWeighedPrice: Long): OrderInfo[S] =
    Impl(4, side, amount, price, matcherFee, matcherFeeAssetId, timestamp, status, assetPair, orderType, avgWeighedPrice)

  def v4[S <: OrderStatus](ao: AcceptedOrder, status: S): OrderInfo[S] = {
    import ao.order
    val acceptedOrderType = if (ao.isLimit) AcceptedOrderType.Limit else AcceptedOrderType.Market
    v4(
      order.orderType,
      order.amount,
      order.price,
      order.matcherFee,
      order.feeAsset,
      order.timestamp,
      status,
      order.assetPair,
      acceptedOrderType,
      ao.fillingInfo.avgWeighedPrice
    )
  }

  private case class Impl[+S <: OrderStatus](version: Byte,
                                             side: OrderType,
                                             amount: Long,
                                             price: Long,
                                             matcherFee: Long,
                                             feeAsset: Asset,
                                             timestamp: Long,
                                             status: S,
                                             assetPair: AssetPair,
                                             orderType: AcceptedOrderType,
                                             avgWeighedPrice: Long)
      extends OrderInfo[S]

  def encode(oi: FinalOrderInfo): Array[Byte] = oi.version match {
    case x if x <= 1 => encodeV1(oi)
    case 2           => encodeV2(oi)
    case 3           => encodeV3(oi)
    case 4           => encodeV4(oi)
    case x           => throw new IllegalArgumentException(s"An unknown order version: $x")
  }

  def decode(bytes: Array[Byte]): FinalOrderInfo = {
    val buf = ByteBuffer.wrap(bytes)
    buf.get match {
      case side @ (0 | 1) => decodeV1(side, buf)
      case 2              => decodeV2(buf)
      case 3              => decodeV3(buf)
      case 4              => decodeV4(buf)
      case x              => throw new IllegalStateException(s"An unknown version of order info: $x")
    }
  }

  private def encodeV1(oi: FinalOrderInfo): Array[Byte] = {
    val assetPairBytes = oi.assetPair.bytes
    ByteBuffer
      .allocate(42 + assetPairBytes.length)
      .put(oi.side.bytes)
      .putLong(oi.amount)
      .putLong(oi.price)
      .putLong(oi.timestamp)
      .putFinalOrderStatus(1, oi.status)
      .putAssetId(oi.assetPair.amountAsset)
      .putAssetId(oi.assetPair.priceAsset)
      .array()
  }

  private def decodeV1(side: Byte, buf: ByteBuffer): FinalOrderInfo = {

    val version: Byte = 1
    val totalAmount   = buf.getLong()
    val totalFee      = 300000L

    OrderInfo.v1(
      side = OrderType(side),
      amount = totalAmount,
      price = buf.getLong,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(version, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId)
    )
  }

  private def encodeVersioned(version: Byte, size: Int, oi: FinalOrderInfo): ByteBuffer =
    // DON'T WRITE BYTES "oi.feeAsset.byteRepr" to the buffer. It works another way :(
    ByteBuffer
      .allocate(size)
      .put(version)
      .put(oi.side.bytes)
      .putLong(oi.amount)
      .putLong(oi.price)
      .putLong(oi.matcherFee)
      .putAssetId(oi.feeAsset)
      .putLong(oi.timestamp)
      .putFinalOrderStatus(version, oi.status)
      .putAssetId(oi.assetPair.amountAsset)
      .putAssetId(oi.assetPair.priceAsset)

  private def encodeV2(oi: FinalOrderInfo): Array[Byte] =
    encodeVersioned(
      version = 2: Byte,
      size = 51 + oi.feeAsset.byteRepr.length + oi.assetPair.bytes.length,
      oi = oi
    ).array()

  private def decodeV2(buf: ByteBuffer): FinalOrderInfo = {

    val side        = OrderType(buf.get)
    val totalAmount = buf.getLong
    val price       = buf.getLong
    val totalFee    = buf.getLong

    OrderInfo.v2(
      side = side,
      amount = totalAmount,
      price = price,
      matcherFee = totalFee,
      matcherFeeAssetId = buf.getAssetId,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(2, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId)
    )
  }

  private def encodeV3(oi: FinalOrderInfo): Array[Byte] =
    encodeVersioned(
      version = 3: Byte,
      size = 52 + oi.feeAsset.byteRepr.length + oi.assetPair.bytes.length,
      oi = oi
    ).putAcceptedOrderType(oi.orderType)
      .array()

  private def decodeV3(buf: ByteBuffer): FinalOrderInfo = {

    val side        = OrderType(buf.get)
    val totalAmount = buf.getLong
    val price       = buf.getLong
    val totalFee    = buf.getLong

    OrderInfo.v3(
      side = side,
      amount = totalAmount,
      price = price,
      matcherFee = totalFee,
      matcherFeeAssetId = buf.getAssetId,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(3, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId),
      orderType = buf.getAcceptedOrderType
    )
  }

  private def encodeV4(oi: OrderInfo.FinalOrderInfo): Array[Byte] = {
    val size: Int = 52 + oi.feeAsset.byteRepr.length + oi.assetPair.bytes.length + 8
    encodeVersioned(4, size, oi)
      .putAcceptedOrderType(oi.orderType)
      .put(Longs.toByteArray(oi.avgWeighedPrice))
      .array()
  }

  private def decodeV4(buf: ByteBuffer): FinalOrderInfo = {

    val side        = OrderType(buf.get)
    val totalAmount = buf.getLong
    val price       = buf.getLong
    val totalFee    = buf.getLong

    OrderInfo.v4(
      side = side,
      amount = totalAmount,
      price = price,
      matcherFee = totalFee,
      matcherFeeAssetId = buf.getAssetId,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(3, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId),
      orderType = buf.getAcceptedOrderType,
      avgWeighedPrice = buf.getLong
    )
  }
}
