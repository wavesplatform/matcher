package com.wavesplatform.dex.collections

import cats.instances.map._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}

sealed abstract case class NegativeMap[K, V] private (xs: Map[K, V]) extends SafeMapOps[K, V, NegativeMap, NegativeMap[K, V]] {
  override protected val factory = NegativeMap
}

object NegativeMap extends SafeMapOpsFactory[NegativeMap] {

  implicit def negativeMapMonoid[K, V: Semigroup]: Monoid[NegativeMap[K, V]] = new Monoid[NegativeMap[K, V]] {
    override def empty: NegativeMap[K, V] = safeMk(Map.empty)
    override def combine(x: NegativeMap[K, V], y: NegativeMap[K, V]): NegativeMap[K, V] = safeMk(x.xs |+| y.xs)
  }

  override protected[collections] def safeMk[K, V](xs: Map[K, V]): NegativeMap[K, V] = new NegativeMap(xs) {}
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.lt(v, n.zero) // v < 0
}
