package com.wavesplatform.dex

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price

import scala.collection.immutable.{Queue, TreeMap}

package object model {
  type Level                 = Queue[LimitOrder]
  type OrderBookSideSnapshot = Map[Price, Seq[LimitOrder]]

  type Side = TreeMap[Price, Level]
  implicit class SideExt(val side: Side) extends AnyVal {

    /** Returns the best limit order in this side and the price of its level */
    def best: Option[(Price, LimitOrder)] = side.headOption.flatMap { case (levelPrice, level) => level.headOption.map(levelPrice -> _) }

    // TODO Replce getOrElse with apply+default
    def enqueue(levelPrice: Price, lo: LimitOrder): Side = side.updated(levelPrice, side.getOrElse(levelPrice, Queue.empty).enqueue(lo))

    final def withoutBest: Option[(Side, (Price, LimitOrder))] = side.headOption.map {
      case (price, level) =>
        val updatedSide = if (level.length == 1) side - price else side.updated(price, level.tail)
        (updatedSide, (price, level.head))
    }

    def replaceBest(newBest: LimitOrder): (Side, LimitOrder) = {
      require(side.nonEmpty, "Cannot replace the best level of an empty side")
      val (price, level) = side.head
      require(level.nonEmpty, "Cannot replace the best element of an empty level")
      val oldHead     = level.head
      val updatedSide = side.updated(price, newBest +: level.tail)
      (updatedSide, oldHead)
    }

    // TODO
    def remove(price: Price, orderId: ByteStr): (Side, LimitOrder) = {
      val (toRemove, toKeep) = side.getOrElse(price, Queue.empty).partition(_.order.id() == orderId)
      require(toRemove.nonEmpty, s"Order $orderId not found at $price")
      val updatedSide = if (toKeep.isEmpty) side - price else side.updated(price, toKeep)
      (updatedSide, toRemove.head)
    }

    def put(price: Price, lo: LimitOrder): Side = side.updated(price, side.getOrElse(price, Queue.empty).enqueue(lo))

    def aggregated: Iterable[LevelAgg] = for { (p, l) <- side.view if l.nonEmpty } yield LevelAgg(l.map(_.amount).sum, p)
    def bestLevel: Option[LevelAgg]    = aggregated.headOption
  }
}
