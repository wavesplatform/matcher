package com.wavesplatform.dex.queue

import com.google.common.primitives.Longs
import com.wavesplatform.dex.queue.ValidatedCommand.{CancelOrder, DeleteOrderBook, PlaceMarketOrder, PlaceOrder}

case class ValidatedCommandWithMeta(offset: ValidatedCommandWithMeta.Offset, timestamp: Long, command: ValidatedCommand) {

  override def toString: String = {
    val eventStr = command match {
      case PlaceOrder(lo) => s"PlaceOrder(${lo.order.idStr()})"
      case PlaceMarketOrder(mo) => s"PlaceMarketOrder(${mo.order.idStr()}, k=${mo.order.assetPair.key}, afs=${mo.availableForSpending})"
      case CancelOrder(assetPair, id, source) => s"CancelOrder($id, ${assetPair.key}, $source)"
      case DeleteOrderBook(p) => s"DeleteOrderBook(${p.key})"
    }
    s"ValidatedCommandWithMeta(offset=$offset, ts=$timestamp, $eventStr)"
  }

}

object ValidatedCommandWithMeta {

  type Offset = Long

  def toBytes(x: ValidatedCommandWithMeta): Array[Byte] =
    Longs.toByteArray(x.offset) ++ Longs.toByteArray(x.timestamp) ++ ValidatedCommand.toBytes(x.command)

  def fromBytes(xs: Array[Byte]): ValidatedCommandWithMeta = ValidatedCommandWithMeta(
    offset = Longs.fromByteArray(xs.take(8)),
    timestamp = Longs.fromByteArray(xs.slice(8, 16)),
    command = ValidatedCommand.fromBytes(xs.drop(16))
  )

}
