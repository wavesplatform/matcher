package com.wavesplatform.dex.codecs

import java.nio.ByteBuffer
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.AcceptedOrderType
import com.wavesplatform.dex.model.OrderStatus

object ByteBufferCodecs {

  implicit class ByteBufferExt(val b: ByteBuffer) extends AnyVal {

    def getBytes: Array[Byte] = {
      val len = b.getInt
      if (b.limit() < len || len < 0)
        throw new Exception(s"Invalid array size ($len)")
      val bytes = new Array[Byte](len)
      b.get(bytes)
      bytes
    }

    def putAssetId(assetId: Asset): ByteBuffer = assetId match {
      case Waves => b.put(0.toByte)
      case IssuedAsset(aid) =>
        require(aid.arr.length < Byte.MaxValue, "Asset ID is too long")
        b.put(aid.arr.length.toByte).put(aid.arr)
    }

    def getAssetId: Asset = b.get() match {
      case 0 => Waves
      case len =>
        val arr = new Array[Byte](len)
        b.get(arr)
        IssuedAsset(ByteStr(arr))
    }

    def putFinalOrderStatus(orderInfoVersion: Byte, st: OrderStatus): ByteBuffer = {
      val tpe: Byte = st match {
        case _: OrderStatus.Filled => 0
        case _: OrderStatus.Cancelled => 1
        case x => throw new IllegalArgumentException(s"Can't encode order status $x")
      }
      val r = b.put(tpe).putLong(st.filledAmount)
      if (orderInfoVersion <= 1) r else r.putLong(st.filledFee)
    }

    def getFinalOrderStatus(orderInfoVersion: Byte, totalAmount: Long, totalFee: Long): OrderStatus.Final = {
      def fee(filledAmount: Long) = if (orderInfoVersion <= 1) (BigInt(filledAmount) * totalFee / totalAmount).toLong else b.getLong

      b.get match {
        case 0 =>
          val filledAmount = b.getLong
          OrderStatus.Filled(filledAmount, fee(filledAmount))
        case 1 =>
          val filledAmount = b.getLong
          OrderStatus.Cancelled(filledAmount, fee(filledAmount))
        case x => throw new IllegalArgumentException(s"Can't parse order status: $x")
      }
    }

    def putAcceptedOrderType(x: AcceptedOrderType): ByteBuffer = x match {
      case AcceptedOrderType.Limit => b.put(0: Byte)
      case AcceptedOrderType.Market => b.put(1: Byte)
    }

    def getAcceptedOrderType: AcceptedOrderType = b.get match {
      case 0 => AcceptedOrderType.Limit
      case 1 => AcceptedOrderType.Market
      case x => throw new IllegalArgumentException(s"Can't parse accepted order type: $x")
    }

  }

}
