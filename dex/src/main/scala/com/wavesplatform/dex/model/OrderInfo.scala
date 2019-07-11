package com.wavesplatform.dex.model

import java.nio.ByteBuffer

import com.wavesplatform.dex.util.Codecs.ByteBufferExt
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

case class OrderInfo[+S <: OrderStatus](version: Byte,
                                        side: OrderType,
                                        amount: Long,
                                        price: Long,
                                        matcherFee: Long,
                                        matcherFeeAssetId: Asset,
                                        timestamp: Long,
                                        status: S,
                                        assetPair: AssetPair)

object OrderInfo {
  type FinalOrderInfo = OrderInfo[OrderStatus.Final]

  def encode(oi: FinalOrderInfo): Array[Byte] = if (oi.version <= 1) encodeV1(oi) else encodeV2(oi)

  def decode(bytes: Array[Byte]): FinalOrderInfo = {
    val buf = ByteBuffer.wrap(bytes)
    buf.get match {
      case side @ (0 | 1) => decodeV1(side, buf)
      case 2              => decodeV2(buf)
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

    OrderInfo(
      version = version,
      side = OrderType(side),
      amount = totalAmount,
      price = buf.getLong,
      matcherFee = totalFee,
      matcherFeeAssetId = Waves,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(version, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId)
    )
  }

  private def encodeV2(oi: FinalOrderInfo): Array[Byte] =
    // DON'T WRITE BYTES "oi.matcherFeeAssetId.byteRepr" to the buffer. It works another way :(
    ByteBuffer
      .allocate(51 + oi.matcherFeeAssetId.byteRepr.length + oi.assetPair.bytes.length)
      .put(oi.version)
      .put(oi.side.bytes)
      .putLong(oi.amount)
      .putLong(oi.price)
      .putLong(oi.matcherFee)
      .putAssetId(oi.matcherFeeAssetId)
      .putLong(oi.timestamp)
      .putFinalOrderStatus(2, oi.status)
      .putAssetId(oi.assetPair.amountAsset)
      .putAssetId(oi.assetPair.priceAsset)
      .array()

  private def decodeV2(buf: ByteBuffer): FinalOrderInfo = {
    val version: Byte = 2
    val side          = OrderType(buf.get)
    val totalAmount   = buf.getLong
    val price         = buf.getLong
    val totalFee      = buf.getLong

    OrderInfo(
      version = version,
      side = side,
      amount = totalAmount,
      price = price,
      matcherFee = totalFee,
      matcherFeeAssetId = buf.getAssetId,
      timestamp = buf.getLong,
      status = buf.getFinalOrderStatus(version, totalAmount, totalFee),
      assetPair = AssetPair(buf.getAssetId, buf.getAssetId)
    )
  }
}
