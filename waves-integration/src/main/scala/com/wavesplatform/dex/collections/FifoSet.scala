package com.wavesplatform.dex.collections

import scala.collection.mutable

sealed trait FifoSet[T] {

  def contains(x: T): Boolean

  /**
   * @return added?
   */
  def append(x: T): Boolean

  def remove(x: T): Boolean

  def clear(): Unit
}

// DEX-1044
private case class LimitedFifoSet[T] private (elements: mutable.LinkedHashSet[T], capacity: Int) extends FifoSet[T] {

  override def contains(id: T): Boolean = elements.contains(id)

  override def append(id: T): Boolean =
    if (contains(id)) false
    else {
      elements.add(id)
      if (elements.size > capacity) elements.headOption.map(elements.remove)
      true
    }

  override def remove(id: T): Boolean =
    elements.remove(id)

  override def clear(): Unit =
    elements.clear()

}

object FifoSet {

  def limited[T](capacity: Int): FifoSet[T] = {
    require(capacity >= 0)
    LimitedFifoSet[T](new mutable.LinkedHashSet[T], capacity)
  }

}
