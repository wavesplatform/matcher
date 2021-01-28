package com.wavesplatform.dex.fp

import cats.Group
import cats.instances.map.catsKernelStdMonoidForMap
import cats.kernel.Semigroup

// TODO DEX-994 This package
object MapImplicits {

  implicit def group[K, V](implicit vGroup: Group[V]): Group[Map[K, V]] = new Group[Map[K, V]] {
    override def inverse(a: Map[K, V]): Map[K, V] = a.map { case (k, v) => k -> vGroup.inverse(v) }
    override def empty: Map[K, V] = Map.empty
    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = catsKernelStdMonoidForMap[K, V].combine(x, y)
  }

  /**
   * @return ∀ (k, v) ∈ A |+| B, v != 0
   */
  implicit def cleaningGroup[K, V](implicit vGroup: Group[V]): Group[Map[K, V]] = new Group[Map[K, V]] {

    override def inverse(a: Map[K, V]): Map[K, V] = a.map { case (k, v) => k -> vGroup.inverse(v) }
    override def empty: Map[K, V] = Map.empty

    override def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] = {
      val (lessXs, biggerXs) = if (xs.size <= ys.size) (xs, ys) else (ys, xs)
      nonEmpty(lessXs).foldLeft(nonEmpty(biggerXs)) {
        case (r, (k, v)) =>
          val updatedV = Semigroup.maybeCombine(v, r.get(k))
          if (updatedV == vGroup.empty) r - k
          else r.updated(k, updatedV)
      }
    }

    private def nonEmpty(xs: Map[K, V]): Map[K, V] = {
      val empty = Group[V].empty
      xs.filterNot { case (_, v) => v == empty }
    }

  }

  implicit final class MapNumericOps[K, V](val self: Map[K, V])(implicit numeric: Numeric[V]) {
    def appendIfNonZero(k: K, v: V): Map[K, V] = if (v == numeric.zero) self else self.updated(k, v)
    def appendIfNonZeroMany(kv: (K, V)*): Map[K, V] = kv.foldLeft(self) { case (r, (k, v)) => r.appendIfNonZero(k, v) }
  }

  implicit final class MapOps[K, V](val self: Map[K, V]) extends AnyVal {
    def appendIfDefined(k: K, v: Option[V]): Map[K, V] = v.fold(self)(self.updated(k, _))
    def appendIfDefinedMany(kv: (K, Option[V])*): Map[K, V] = kv.foldLeft(self) { case (r, (k, v)) => r.appendIfDefined(k, v) }
    def filterByMap(other: Map[K, V]): Map[K, V] = self.view.filterKeys(other.contains).toMap
  }

}
