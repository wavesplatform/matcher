package com.wavesplatform.dex.collections

import cats.instances.map._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}

sealed abstract case class PositiveMap[K, V] private (xs: Map[K, V]) extends SafeMapOps[K, V, PositiveMap, PositiveMap[K, V]] {
  override protected val factory = PositiveMap
}

object PositiveMap extends SafeMapOpsFactory[PositiveMap] {

  implicit def positiveMapMonoid[K, V: Semigroup]: Monoid[PositiveMap[K, V]] = new Monoid[PositiveMap[K, V]] {
    override def empty: PositiveMap[K, V] = safeMk(Map.empty)
    override def combine(x: PositiveMap[K, V], y: PositiveMap[K, V]): PositiveMap[K, V] = safeMk(x.xs |+| y.xs)
  }

  override protected[collections] def safeMk[K, V](xs: Map[K, V]): PositiveMap[K, V] = new PositiveMap(xs) {}
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.gt(v, n.zero) // v > 0
}
