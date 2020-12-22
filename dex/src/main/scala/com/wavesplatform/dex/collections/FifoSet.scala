package com.wavesplatform.dex.collections

sealed trait FifoSet[T] {
  def contains(x: T): Boolean
  def append(x: T): FifoSet[T]
}

private case class LimitedFifoSet[T] private (elements: Set[T], queue: Vector[T], capacity: Int) extends FifoSet[T] {
  def contains(id: T): Boolean = elements.contains(id)

  def append(id: T): LimitedFifoSet[T] =
    if (contains(id)) this
    else if (capacity <= 0) queue match {
      case h +: t => copy(elements - h + id, t :+ id)
      case _ => this
    }
    else copy(elements + id, queue :+ id, capacity - 1)

}

object FifoSet {

  def limited[T](capacity: Int): FifoSet[T] = {
    require(capacity >= 0)
    LimitedFifoSet[T](Set.empty, Vector.empty, capacity)
  }

}
