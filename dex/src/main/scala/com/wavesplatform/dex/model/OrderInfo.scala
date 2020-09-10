package com.wavesplatform.dex.model

import java.math.{BigDecimal, BigInteger, RoundingMode}
import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import com.wavesplatform.dex.codecs.ByteBufferCodecs.ByteBufferExt
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.{Order, OrderType}

sealed trait OrderInfo[+S <: OrderStatus] {
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
  def orderVersion: Byte
  def totalExecutedPriceAssets: Long
}

object OrderInfo {
  type FinalOrderInfo = OrderInfo[OrderStatus.Final]

  private def backwardCompatibleAvgWeighedPrice(status: OrderStatus, price: Long): Long =
    if (status == OrderStatus.Accepted || status == OrderStatus.NotFound) 0 else price

  private def backwardCompatibleOrderVersion(infoVersion: Byte, feeAsset: Asset): Byte = (infoVersion, feeAsset) match {
    case (1, _)     => 1
    case (2, Waves) => 2
    case _          => 3
  }

  private[model] def getTotalExecutedPriceAssets(filledAmount: Long, avgWeighedPrice: Long): Long =
    new BigDecimal(avgWeighedPrice)
      .multiply(new BigDecimal(filledAmount))
      .scaleByPowerOfTen(-Order.PriceConstantExponent)
      .setScale(0, RoundingMode.FLOOR)
      .longValue()

  def v1[S <: OrderStatus](side: OrderType, amount: Long, price: Long, timestamp: Long, status: S, assetPair: AssetPair): OrderInfo[S] = {

    val version: Byte   = 1
    val feeAsset        = Waves
    val avgWeighedPrice = backwardCompatibleAvgWeighedPrice(status, price)

    Impl(
      version = version,
      side = side,
      amount = amount,
      price = price,
      matcherFee = 300000L,
      feeAsset = feeAsset,
      timestamp = timestamp,
      status = status,
      assetPair = assetPair,
      orderType = AcceptedOrderType.Limit,
      avgWeighedPrice = avgWeighedPrice,
      orderVersion = backwardCompatibleOrderVersion(version, feeAsset),
      totalExecutedPriceAssets = getTotalExecutedPriceAssets(status.filledAmount, avgWeighedPrice)
    )
  }

  def v2[S <: OrderStatus](order: Order, status: S): OrderInfo[S] =
    v2(order.orderType, order.amount, order.price, order.matcherFee, order.feeAsset, order.timestamp, status, order.assetPair)

  def v2[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair): OrderInfo[S] = {
    val version: Byte   = 2
    val avgWeighedPrice = backwardCompatibleAvgWeighedPrice(status, price)

    Impl(
      version = version,
      side = side,
      amount = amount,
      price = price,
      matcherFee = matcherFee,
      feeAsset = matcherFeeAssetId,
      timestamp = timestamp,
      status = status,
      assetPair = assetPair,
      orderType = AcceptedOrderType.Limit,
      avgWeighedPrice = avgWeighedPrice,
      orderVersion = backwardCompatibleOrderVersion(version, matcherFeeAssetId),
      totalExecutedPriceAssets = getTotalExecutedPriceAssets(status.filledAmount, avgWeighedPrice)
    )
  }

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
                           orderType: AcceptedOrderType): OrderInfo[S] = {
    val version: Byte   = 3
    val avgWeighedPrice = backwardCompatibleAvgWeighedPrice(status, price)

    Impl(
      version = version,
      side = side,
      amount = amount,
      price = price,
      matcherFee = matcherFee,
      feeAsset = matcherFeeAssetId,
      timestamp = timestamp,
      status = status,
      assetPair = assetPair,
      orderType = orderType,
      avgWeighedPrice = avgWeighedPrice,
      orderVersion = backwardCompatibleOrderVersion(version, matcherFeeAssetId),
      totalExecutedPriceAssets = getTotalExecutedPriceAssets(status.filledAmount, avgWeighedPrice)
    )
  }

  def v4[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair,
                           orderType: AcceptedOrderType,
                           avgWeighedPrice: Long): OrderInfo[S] = {
    val version: Byte = 4
    Impl(
      version = version,
      side = side,
      amount = amount,
      price = price,
      matcherFee = matcherFee,
      feeAsset = matcherFeeAssetId,
      timestamp = timestamp,
      status = status,
      assetPair = assetPair,
      orderType = orderType,
      avgWeighedPrice = avgWeighedPrice,
      orderVersion = backwardCompatibleOrderVersion(version, matcherFeeAssetId),
      totalExecutedPriceAssets = getTotalExecutedPriceAssets(status.filledAmount, avgWeighedPrice)
    )
  }

  def v4[S <: OrderStatus](ao: AcceptedOrder, status: S): OrderInfo[S] = {
    import ao.order
    val acceptedOrderType = if (ao.isLimit) AcceptedOrderType.Limit else AcceptedOrderType.Market
    v4(
      side = order.orderType,
      amount = order.amount,
      price = order.price,
      matcherFee = order.matcherFee,
      matcherFeeAssetId = order.feeAsset,
      timestamp = order.timestamp,
      status = status,
      assetPair = order.assetPair,
      orderType = acceptedOrderType,
      avgWeighedPrice = ao.fillingInfo.avgWeighedPrice
    )
  }

  def v5[S <: OrderStatus](ao: AcceptedOrder, status: S): OrderInfo[S] = {
    import ao.order
    v5(
      side = order.orderType,
      amount = order.amount,
      price = order.price,
      matcherFee = order.matcherFee,
      matcherFeeAssetId = order.feeAsset,
      timestamp = order.timestamp,
      status = status,
      assetPair = order.assetPair,
      orderType = ao.orderType,
      avgWeighedPrice = ao.fillingInfo.avgWeighedPrice,
      orderVersion = ao.order.version
    )
  }

  def v5[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair,
                           orderType: AcceptedOrderType,
                           avgWeighedPrice: Long,
                           orderVersion: Byte): OrderInfo[S] =
    Impl(
      version = 5,
      side = side,
      amount = amount,
      price = price,
      matcherFee = matcherFee,
      feeAsset = matcherFeeAssetId,
      timestamp = timestamp,
      status = status,
      assetPair = assetPair,
      orderType = orderType,
      avgWeighedPrice = avgWeighedPrice,
      orderVersion = orderVersion,
      totalExecutedPriceAssets = getTotalExecutedPriceAssets(status.filledAmount, avgWeighedPrice)
    )

  def v6[S <: OrderStatus](ao: AcceptedOrder, status: S): OrderInfo[S] = {
    import ao.order
    v6(
      side = order.orderType,
      amount = order.amount,
      price = order.price,
      matcherFee = order.matcherFee,
      matcherFeeAssetId = order.feeAsset,
      timestamp = order.timestamp,
      status = status,
      assetPair = order.assetPair,
      orderType = ao.orderType,
      orderVersion = ao.order.version,
      totalExecutedPriceAssets = ao.fillingInfo.totalExecutedPriceAssets
    )
  }

  def v6[S <: OrderStatus](side: OrderType,
                           amount: Long,
                           price: Long,
                           matcherFee: Long,
                           matcherFeeAssetId: Asset,
                           timestamp: Long,
                           status: S,
                           assetPair: AssetPair,
                           orderType: AcceptedOrderType,
                           orderVersion: Byte,
                           totalExecutedPriceAssets: Long): OrderInfo[S] = {
    val avgWeighedPrice =
      if (status.filledAmount == 0) 0L
      else BigInteger.valueOf(totalExecutedPriceAssets).divide(BigInteger.valueOf(status.filledAmount)).longValueExact()

    Impl(
      version = 6,
      side = side,
      amount = amount,
      price = price,
      matcherFee = matcherFee,
      feeAsset = matcherFeeAssetId,
      timestamp = timestamp,
      status = status,
      assetPair = assetPair,
      orderType = orderType,
      avgWeighedPrice = avgWeighedPrice,
      orderVersion = orderVersion,
      totalExecutedPriceAssets = totalExecutedPriceAssets
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
                                             avgWeighedPrice: Long,
                                             orderVersion: Byte,
                                             totalExecutedPriceAssets: Long)
      extends OrderInfo[S]

  def encode(oi: FinalOrderInfo): Array[Byte] = oi.version match {
    case x if x <= 1 => encodeV1(oi)
    case 2           => encodeV2(oi)
    case 3           => encodeV3(oi)
    case 4           => encodeV4(oi)
    case 5           => encodeV5(oi)
    case 6           => encodeV6(oi)
    case x           => throw new IllegalArgumentException(s"An unknown order version: $x")
  }

  def decode(bytes: Array[Byte]): FinalOrderInfo = {
    val buf = ByteBuffer.wrap(bytes)
    buf.get match {
      case side @ (0 | 1) => decodeV1(side, buf)
      case 2              => decodeV2(buf)
      case 3              => decodeV3(buf)
      case 4              => decodeV4(buf)
      case 5              => decodeV5(buf)
      case 6              => decodeV6(buf)
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

  private def encodeV5(oi: OrderInfo.FinalOrderInfo): Array[Byte] = {
    val size: Int = 52 + oi.feeAsset.byteRepr.length + oi.assetPair.bytes.length + 8 + 1
    encodeVersioned(5, size, oi)
      .putAcceptedOrderType(oi.orderType)
      .put(Longs.toByteArray(oi.avgWeighedPrice))
      .put(oi.orderVersion)
      .array()
  }

  private def decodeV5(buf: ByteBuffer): FinalOrderInfo = {

    val side        = OrderType(buf.get)
    val totalAmount = buf.getLong
    val price       = buf.getLong
    val totalFee    = buf.getLong

    OrderInfo.v5(
      side = side,
      amount = totalAmount,
      price = price,
      matcherFee = totalFee,
      matcherFeeAssetId = buf.getAssetId,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(3, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId),
      orderType = buf.getAcceptedOrderType,
      avgWeighedPrice = buf.getLong,
      orderVersion = buf.get()
    )
  }

  private def encodeV6(oi: OrderInfo.FinalOrderInfo): Array[Byte] = {
    val size: Int = 52 + oi.feeAsset.byteRepr.length + oi.assetPair.bytes.length + 1 + 8
    encodeVersioned(6, size, oi)
      .putAcceptedOrderType(oi.orderType)
      .put(oi.orderVersion)
      .put(Longs.toByteArray(oi.totalExecutedPriceAssets))
      .array()
  }

  private def decodeV6(buf: ByteBuffer): FinalOrderInfo = {

    val side        = OrderType(buf.get)
    val totalAmount = buf.getLong
    val price       = buf.getLong
    val totalFee    = buf.getLong

    OrderInfo.v6(
      side = side,
      amount = totalAmount,
      price = price,
      matcherFee = totalFee,
      matcherFeeAssetId = buf.getAssetId,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(3, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId),
      orderType = buf.getAcceptedOrderType,
      orderVersion = buf.get(),
      totalExecutedPriceAssets = buf.getLong()
    )
  }
}
