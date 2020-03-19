package com.wavesplatform.dex.model

import java.math.BigInteger
import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.util.Codecs.ByteBufferExt

import scala.collection.mutable

object OrderBookSideSnapshot {

  def serialize(dest: mutable.ArrayBuilder[Byte], snapshot: OrderBookSideSnapshot): Unit = {
    dest ++= Ints.toByteArray(snapshot.size)
    snapshot.foreach {
      case (price, xs) =>
        dest ++= Longs.toByteArray(price)
        dest ++= Ints.toByteArray(xs.size)
        xs.foreach(serialize(dest, _))
    }
  }

  def fromBytes(bb: ByteBuffer): OrderBookSideSnapshot = {
    val snapshotSize = bb.getInt
    val r            = Map.newBuilder[Price, Seq[LimitOrder]]
    (1 to snapshotSize).foreach { _ =>
      val price       = bb.getLong
      val levelSize   = bb.getInt
      val limitOrders = (1 to levelSize).map(_ => loFromBytes(bb))
      r += price -> limitOrders
    }
    r.result()
  }

  def serializeOld(dest: mutable.ArrayBuilder[Byte], lo: LimitOrder): Unit = {
    dest ++= lo.order.orderType.bytes
    dest ++= Longs.toByteArray(lo.amount)
    dest ++= Longs.toByteArray(lo.fee)
    dest += lo.order.version

    val orderBytes = lo.order.bytes()

    dest ++= Ints.toByteArray(orderBytes.length)
    dest ++= orderBytes
  }

  def serialize(dest: mutable.ArrayBuilder[Byte], lo: LimitOrder): Unit = {

    dest ++= lo.order.orderType.bytes.map(b => (b | 2).toByte)
    dest ++= Longs.toByteArray(lo.amount)
    dest ++= Longs.toByteArray(lo.fee)
    dest += lo.order.version

    val orderBytes               = lo.order.bytes()
    val avgWeighedPriceNominator = lo.avgWeighedPriceNominator.toByteArray

    dest ++= Ints.toByteArray(orderBytes.length)
    dest ++= orderBytes

    dest ++= Ints.toByteArray(avgWeighedPriceNominator.length)
    dest ++= avgWeighedPriceNominator
  }

  def loFromBytes(bb: ByteBuffer): LimitOrder = {

    val header           = bb.get
    val structureVersion = header >> 1
    val orderType        = (header & 1).toByte

    val amount  = bb.getLong
    val fee     = bb.getLong
    val version = bb.get
    val order   = Order.fromBytes(version, bb.getBytes)

    val avgWeighedPriceNominator =
      if (structureVersion == 1) new BigInteger(bb.getBytes)
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
