package com.wavesplatform.dex.codecs

import java.math.BigInteger
import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.codecs.ByteBufferCodecs.ByteBufferExt
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{BuyLimitOrder, LimitOrder, OrderBookSideSnapshot, SellLimitOrder}

import scala.collection.mutable

object OrderBookSideSnapshotCodecs {

  def encode(dest: mutable.ArrayBuilder[Byte], snapshot: OrderBookSideSnapshot): Unit = {
    dest ++= Ints.toByteArray(snapshot.size)
    snapshot.foreach {
      case (price, xs) =>
        dest ++= Longs.toByteArray(price)
        dest ++= Ints.toByteArray(xs.size)
        xs.foreach(encodeLoV2(dest, _))
    }
  }

  def decode(bb: ByteBuffer): OrderBookSideSnapshot = {
    val snapshotSize = bb.getInt
    val r            = Map.newBuilder[Price, Seq[LimitOrder]]
    (1 to snapshotSize).foreach { _ =>
      val price       = bb.getLong
      val levelSize   = bb.getInt
      val limitOrders = (1 to levelSize).map(_ => decodeLo(bb))
      r += price -> limitOrders
    }
    r.result()
  }

  def encodeLoV1(dest: mutable.ArrayBuilder[Byte], lo: LimitOrder): Unit = {

    dest ++= lo.order.orderType.bytes
    dest ++= Longs.toByteArray(lo.amount)
    dest ++= Longs.toByteArray(lo.fee)
    dest += lo.order.version

    val orderBytes = lo.order.bytes()

    dest ++= Ints.toByteArray(orderBytes.length)
    dest ++= orderBytes
  }

  def encodeLoV2(dest: mutable.ArrayBuilder[Byte], lo: LimitOrder): Unit = {
    val avgWeighedPriceNominatorBytes = lo.avgWeighedPriceNominator.toByteArray

    dest += 2

    encodeLoV1(dest, lo)

    dest ++= Ints.toByteArray(avgWeighedPriceNominatorBytes.length)
    dest ++= avgWeighedPriceNominatorBytes
  }

  def decodeLo(bb: ByteBuffer): LimitOrder = {

    val header    = bb.get
    val version   = if (header == 2) 2 else 1
    val orderType = if (version == 1) header else bb.get

    val amount       = bb.getLong
    val fee          = bb.getLong
    val orderVersion = bb.get
    val order        = Order.fromBytes(orderVersion, bb.getBytes)

    val avgWeighedPriceNominator =
      if (version == 2) new BigInteger(bb.getBytes)
      else {
        val filledAmount = order.amount - amount
        (BigInt(order.price) * filledAmount).bigInteger
      }

    OrderType(orderType) match {
      case OrderType.SELL => SellLimitOrder(amount, fee, order, avgWeighedPriceNominator)
      case OrderType.BUY  => BuyLimitOrder(amount, fee, order, avgWeighedPriceNominator)
    }
  }
}
