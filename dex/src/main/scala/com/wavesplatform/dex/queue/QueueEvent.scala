package com.wavesplatform.dex.queue

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.DigestSize
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder}

sealed trait QueueEvent extends Product with Serializable {
  def assetPair: AssetPair
}

object QueueEvent {

  case class Placed(limitOrder: LimitOrder) extends QueueEvent {
    override def assetPair: AssetPair = limitOrder.order.assetPair
  }

  case class PlacedMarket(marketOrder: MarketOrder) extends QueueEvent {
    override def assetPair: AssetPair = marketOrder.order.assetPair
  }

  case class Canceled(assetPair: AssetPair, orderId: Order.Id) extends QueueEvent
  case class OrderBookDeleted(assetPair: AssetPair)            extends QueueEvent

  implicit final class Ops(val self: QueueEvent) extends AnyVal {
    def assets: Set[Asset] = self match {
      case x: Placed           => x.assetPair.assets + x.limitOrder.order.feeAsset
      case x: PlacedMarket     => x.assetPair.assets + x.marketOrder.order.feeAsset
      case x: Canceled         => x.assetPair.assets
      case x: OrderBookDeleted => x.assetPair.assets
    }
  }

  def toBytes(x: QueueEvent): Array[Byte] = x match {
    case Placed(lo)                   => (1: Byte) +: lo.order.version +: lo.order.bytes()
    case Canceled(assetPair, orderId) => (2: Byte) +: (assetPair.bytes ++ orderId.arr)
    case OrderBookDeleted(assetPair)  => (3: Byte) +: assetPair.bytes
    case PlacedMarket(mo)             => (4: Byte) +: Array.concat(Longs.toByteArray(mo.availableForSpending), Array(mo.order.version), mo.order.bytes())
  }

  def fromBytes(xs: Array[Byte]): QueueEvent = xs.head match {
    case 1 => Placed(LimitOrder(Order.fromBytes(xs(1), xs.slice(2, Int.MaxValue))))
    case 2 => Canceled(AssetPair.fromBytes(xs.tail), ByteStr(xs.takeRight(DigestSize)))
    case 3 => OrderBookDeleted(AssetPair.fromBytes(xs.tail))
    case 4 => val afs = Longs.fromByteArray(xs.slice(1, 9)); PlacedMarket(MarketOrder(Order.fromBytes(xs(9), xs.slice(10, Int.MaxValue)), afs))
    case x => throw new IllegalArgumentException(s"Unknown event type: $x")
  }
}
