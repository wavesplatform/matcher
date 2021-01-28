package com.wavesplatform.dex.collections

sealed abstract case class NonPositiveMap[K, V] private (xs: Map[K, V]) extends SafeMapOps[K, V, NonPositiveMap, NonPositiveMap[K, V]] {
  override protected val factory = NonPositiveMap
}

object NonPositiveMap extends SafeMapOpsFactory[NonPositiveMap] {
  override protected[collections] def safeMk[K, V](xs: Map[K, V]): NonPositiveMap[K, V] = new NonPositiveMap(xs) {}
  override protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean = n.lteq(v, n.zero) // v <= 0
}
