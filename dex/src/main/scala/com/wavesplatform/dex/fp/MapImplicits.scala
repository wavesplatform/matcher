package com.wavesplatform.dex.fp

import cats.Group
import cats.instances.map.catsKernelStdMonoidForMap

object MapImplicits {
  implicit def group[K, V](implicit vGroup: Group[V]): Group[Map[K, V]] = new Group[Map[K, V]] {
    override def inverse(a: Map[K, V]): Map[K, V]               = a.map { case (k, v) => k -> vGroup.inverse(v) }
    override def empty: Map[K, V]                               = Map.empty
    override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = catsKernelStdMonoidForMap[K, V].combine(x, y)
  }
}
