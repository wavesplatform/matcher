package com.wavesplatform.dex

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price

import scala.collection.mutable

package object model {
  type Level                 = Vector[LimitOrder]
  type OrderBookSideSnapshot = Map[Price, Seq[LimitOrder]]

  type Side = mutable.TreeMap[Price, Level]
  implicit class SideExt(val side: Side) extends AnyVal {

    /** Returns the best limit order in this side and the price of its level */
    def best: Option[(LimitOrder, Price)] = side.headOption.flatMap { case (levelPrice, level) => level.headOption.map(_ -> levelPrice) }

    final def removeBest(): LimitOrder = side.headOption match {
      case l if l.forall(_._2.isEmpty) => throw new IllegalArgumentException("Cannot remove the best element from an empty level")
      case Some((price, level)) =>
        if (level.length == 1) side -= price
        else side += price -> level.tail
        level.head
    }

    def replaceBest(newBest: LimitOrder): (LimitOrder, Side) = {
      require(side.nonEmpty, "Cannot replace the best level of an empty side")
      val (price, level) = side.head
      require(level.nonEmpty, "Cannot replace the best element of an empty level")
      val oldHead = level.head
      side += (price -> (newBest +: level.tail))
      (oldHead, side)
    }

    def remove(price: Price, orderId: ByteStr): LimitOrder = {
      val (toRemove, toKeep) = side.getOrElse(price, Vector.empty).partition(_.order.id() == orderId)
      require(toRemove.nonEmpty, s"Order $orderId not found at $price")
      if (toKeep.isEmpty) side -= price else side += price -> toKeep

      toRemove.head
    }

    def aggregated: Iterable[LevelAgg] = for { (p, l) <- side.view if l.nonEmpty } yield LevelAgg(l.map(_.amount).sum, p)
    def bestLevel: Option[LevelAgg] = aggregated.headOption
  }
}
