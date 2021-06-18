package com.wavesplatform.dex.model

import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.order.OrderType

import scala.collection.mutable

case class LastTrade(price: Long, amount: Long, side: OrderType) {
  override def toString: String = s"LastTrade(p=$price, a=$amount, $side)"
}

object LastTrade {

  def serialize(dest: mutable.ArrayBuilder[Byte], x: LastTrade): Unit = {
    dest ++= Longs.toByteArray(x.price)
    dest ++= Longs.toByteArray(x.amount)
    dest ++= x.side.bytes
  }

  def fromBytes(bb: ByteBuffer): LastTrade = LastTrade(bb.getLong, bb.getLong, OrderType(bb.get))
}
