package com.wavesplatform.dex.model

import java.nio.ByteBuffer

import scala.collection.mutable

case class OrderBookSnapshot(bids: OrderBookSideSnapshot, asks: OrderBookSideSnapshot, lastTrade: Option[LastTrade])
object OrderBookSnapshot {

  val empty: OrderBookSnapshot = OrderBookSnapshot(bids = Map.empty, asks = Map.empty, None)

  def serialize(dest: mutable.ArrayBuilder[Byte], x: OrderBookSnapshot): Unit = {
    OrderBookSideSnapshot.encode(dest, x.bids)
    OrderBookSideSnapshot.encode(dest, x.asks)
    x.lastTrade match {
      case None => dest += 0
      case Some(lastTrade) =>
        dest += 1
        LastTrade.serialize(dest, lastTrade)
    }
  }

  def fromBytes(bb: ByteBuffer): OrderBookSnapshot =
    OrderBookSnapshot(
      OrderBookSideSnapshot.decode(bb),
      OrderBookSideSnapshot.decode(bb),
      bb.get match {
        case 0 => None
        case 1 => Some(LastTrade.fromBytes(bb))
        case x => throw new RuntimeException(s"Can't deserialize Option as $x")
      }
    )
}
