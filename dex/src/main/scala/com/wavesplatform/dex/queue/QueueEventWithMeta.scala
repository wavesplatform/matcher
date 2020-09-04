package com.wavesplatform.dex.queue

import com.google.common.primitives.Longs

case class QueueEventWithMeta(offset: QueueEventWithMeta.Offset, timestamp: Long, event: QueueEvent) {
  override def toString: String = {
    val eventStr = event match {
      case QueueEvent.Placed(lo)                      => s"Placed(${lo.order.idStr()})"
      case QueueEvent.PlacedMarket(mo)                => s"PlacedMarket(${mo.order.idStr()}, k=${mo.order.assetPair.key}, afs=${mo.availableForSpending})"
      case QueueEvent.Canceled(assetPair, id, source) => s"Canceled($id, ${assetPair.key}, $source)"
      case QueueEvent.OrderBookDeleted(p)             => s"OrderBookDeleted(${p.key})"
    }
    s"QueueEventWithMeta(offset=$offset, ts=$timestamp, $eventStr)"
  }
}

object QueueEventWithMeta {

  type Offset = Long

  def toBytes(x: QueueEventWithMeta): Array[Byte] = Longs.toByteArray(x.offset) ++ Longs.toByteArray(x.timestamp) ++ QueueEvent.toBytes(x.event)

  def fromBytes(xs: Array[Byte]): QueueEventWithMeta = QueueEventWithMeta(
    offset = Longs.fromByteArray(xs.take(8)),
    timestamp = Longs.fromByteArray(xs.slice(8, 16)),
    event = QueueEvent.fromBytes(xs.drop(16))
  )
}
