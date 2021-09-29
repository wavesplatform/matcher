package com.wavesplatform.dex.model

import com.google.common.primitives.Longs

import java.nio.ByteBuffer
import com.wavesplatform.dex.codecs.OrderBookSideSnapshotCodecs

import scala.collection.mutable

case class OrderBookSnapshot(bids: OrderBookSideSnapshot, asks: OrderBookSideSnapshot, lastTrade: Option[LastTrade], nextTxTimestamp: Long)

object OrderBookSnapshot {

  val empty: OrderBookSnapshot = OrderBookSnapshot(bids = Map.empty, asks = Map.empty, None, 0L)

  def serialize(dest: mutable.ArrayBuilder[Byte], x: OrderBookSnapshot): Unit = {
    OrderBookSideSnapshotCodecs.encode(dest, x.bids)
    OrderBookSideSnapshotCodecs.encode(dest, x.asks)
    x.lastTrade match {
      case None => dest += 0
      case Some(lastTrade) =>
        dest += 1
        LastTrade.serialize(dest, lastTrade)
    }
    dest ++= Longs.toByteArray(x.nextTxTimestamp)
  }

  def fromBytes(bb: ByteBuffer): OrderBookSnapshot = {
    val bids = OrderBookSideSnapshotCodecs.decode(bb)
    val asks = OrderBookSideSnapshotCodecs.decode(bb)
    val lastTrade = bb.get match {
      case 0 => None
      case 1 => Some(LastTrade.fromBytes(bb))
      case x => throw new RuntimeException(s"Can't deserialize Option as $x")
    }
    val ts =
      if (bb.hasRemaining) bb.getLong // TODO: remove checking hasRemaining when all snapshots will have ts
      else 0L
    OrderBookSnapshot(bids, asks, lastTrade, ts)
  }

}
