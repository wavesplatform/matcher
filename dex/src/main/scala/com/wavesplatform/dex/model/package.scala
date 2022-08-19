package com.wavesplatform.dex

import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.Order

import scala.collection.immutable.{Queue, TreeMap}

package object model {

  type AssetPairValidator = AssetPair => Boolean

  type Level = Queue[LimitOrder]
  type OrderBookSideSnapshot = Map[Price, Seq[LimitOrder]]

  type Side = TreeMap[Price, Level]

  implicit class SideExt(val side: Side) extends AnyVal {

    /** Returns the best limit order in this side and the price of its level */
    def best: Option[(Price, LimitOrder)] = side.headOption.flatMap { case (levelPrice, level) => level.headOption.map(levelPrice -> _) }

    def enqueue(levelPrice: Price, lo: LimitOrder): Side = {
      val v = side.getOrElse(levelPrice, Queue.empty)
      side.updated(levelPrice, v.enqueue(lo))
    }

    def unsafeWithoutBest: (Side, Order.Id) = side.headOption match {
      case Some((price, level)) =>
        val updated = if (level.length == 1) side - price else side.updated(price, level.tail)
        (updated, level.head.order.id())
      case None => throw new IllegalArgumentException("Expected side to have at least one order")
    }

    def unsafeUpdateBest(updated: LimitOrder): Side = {
      require(side.nonEmpty, "Cannot replace the best level of an empty side")
      val (price, level) = side.head
      require(level.nonEmpty, "Cannot replace the best element of an empty level")
      val oldHead = level.head
      require(oldHead.order.id() == updated.order.id(), "Expected the same order")
      side.updated(price, updated +: level.tail)
    }

    def unsafeRemove(price: Price, orderId: ByteStr): (Side, LimitOrder) = {
      val v = side.getOrElse(price, Queue.empty)
      val (toRemove, toKeep) = v.partition(_.order.id() == orderId)
      require(toRemove.nonEmpty, s"Order $orderId not found at $price")
      val updatedSide = if (toKeep.isEmpty) side - price else side.updated(price, toKeep)
      (updatedSide, toRemove.head)
    }

    def put(price: Price, lo: LimitOrder): Side = {
      val v = side.getOrElse(price, Queue.empty)
      side.updated(price, v.enqueue(lo))
    }

    def aggregated: Iterable[LevelAgg] = for { (p, l) <- side.view if l.nonEmpty } yield LevelAgg(l.map(_.amount).sum, p)
    def bestLevel: Option[LevelAgg] = aggregated.headOption
  }

}
