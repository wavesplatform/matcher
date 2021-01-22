package com.wavesplatform.dex.collections

class PositiveMap[K, V] private (val xs: Map[K, V]) extends SafeMapOps[K, V, PositiveMap, PositiveMap[K, V]] {
  override protected val factory = PositiveMap
}

object PositiveMap extends SafeMapOpsFactory[PositiveMap] {
  override protected[collections] def safeMk[K, V](xs: Map[K, V]): PositiveMap[K, V] = new PositiveMap(xs)
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.gt(v, n.zero) // v > 0
}
