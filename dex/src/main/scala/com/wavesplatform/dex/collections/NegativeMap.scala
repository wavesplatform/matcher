package com.wavesplatform.dex.collections

class NegativeMap[K, V] private (val xs: Map[K, V]) extends SafeMapOps[K, V, NegativeMap, NegativeMap[K, V]] {
  override protected val factory = NegativeMap
}

object NegativeMap extends SafeMapOpsFactory[NegativeMap] {
  override protected[collections] def safeMk[K, V](xs: Map[K, V]): NegativeMap[K, V] = new NegativeMap(xs)
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.lt(v, n.zero) // v < 0
}
