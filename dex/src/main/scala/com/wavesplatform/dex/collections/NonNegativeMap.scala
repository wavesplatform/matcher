package com.wavesplatform.dex.collections

sealed abstract case class NonNegativeMap[K, V] private (xs: Map[K, V]) extends SafeMapOps[K, V, NonNegativeMap, NonNegativeMap[K, V]] {
  override protected val factory = NonNegativeMap
}

object NonNegativeMap extends SafeMapOpsFactory[NonNegativeMap] {
  override protected[collections] def safeMk[K, V](xs: Map[K, V]): NonNegativeMap[K, V] = new NonNegativeMap(xs) {}
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.gteq(v, n.zero) // v >= 0
}
