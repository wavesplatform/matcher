package com.wavesplatform.dex.model

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

  def serialize(dest: mutable.ArrayBuilder[Byte], lo: LimitOrder): Unit = {

    dest ++= lo.order.orderType.bytes
    dest ++= Longs.toByteArray(lo.amount)
    dest ++= Longs.toByteArray(lo.fee)
    dest += lo.order.version

    val orderBytes = lo.order.bytes()

    dest ++= Ints.toByteArray(orderBytes.length)
    dest ++= orderBytes
  }

  def loFromBytes(bb: ByteBuffer): LimitOrder = {

    val orderType = OrderType(bb.get)
    val amount    = bb.getLong
    val fee       = bb.getLong
    val version   = bb.get
    val order     = Order.fromBytes(version, bb.getBytes)

    orderType match {
      case OrderType.SELL => SellLimitOrder(amount, fee, order)
      case OrderType.BUY  => BuyLimitOrder(amount, fee, order)
    }
  }
}
