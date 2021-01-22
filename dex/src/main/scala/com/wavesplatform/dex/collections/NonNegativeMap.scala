package com.wavesplatform.dex.collections

class NonNegativeMap[K, V] private(val xs: Map[K, V]) extends SafeMapOps[K, V, NonNegativeMap, NonNegativeMap[K, V]] {
  override protected val factory = NonNegativeMap
}

object NonNegativeMap extends SafeMapOpsFactory[NonNegativeMap] {
  override protected[collections] def safeMk[K, V](xs: Map[K, V]): NonNegativeMap[K, V] = new NonNegativeMap(xs)
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.gt(v, n.zero) // v >= 0
}
