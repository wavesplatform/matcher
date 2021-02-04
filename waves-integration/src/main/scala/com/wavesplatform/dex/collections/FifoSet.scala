package com.wavesplatform.dex.collections

sealed trait FifoSet[T] {
  def contains(x: T): Boolean

  /**
   * @return (updated, added?)
   */
  def append(x: T): (FifoSet[T], Boolean)
}

// DEX-1044
private case class LimitedFifoSet[T] private (elements: Set[T], queue: Vector[T], capacity: Int) extends FifoSet[T] {
  def contains(id: T): Boolean = elements.contains(id)

  def append(id: T): (LimitedFifoSet[T], Boolean) =
    if (contains(id)) (this, false)
    else if (capacity <= 0) queue match {
      case h +: t => (copy(elements - h + id, t :+ id), true)
      case _ => (this, false)
    }
    else (copy(elements + id, queue :+ id, capacity - 1), true)

}

object FifoSet {

  def limited[T](capacity: Int): FifoSet[T] = {
    require(capacity >= 0)
    LimitedFifoSet[T](Set.empty, Vector.empty, capacity)
  }

}
