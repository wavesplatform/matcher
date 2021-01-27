package com.wavesplatform.dex.collections

trait SafeMapOps[K, V, CC[KK, VV] <: SafeMapOps[KK, VV, CC, _], C <: SafeMapOps[K, V, CC, C]] {
  val xs: Map[K, V]

  def keySet: Set[K] = xs.keySet

  def contains(k: K): Boolean = xs.contains(k)
  def getOrElse(k: K, v: => V): V = xs.getOrElse(k, v)
  def collect[W](pf: PartialFunction[(K, V), W]): Iterable[W] = xs.collect(pf)
  def filter(pred: ((K, V)) => Boolean): Map[K, V] = xs.filter(pred)
  def foreach(f: ((K, V)) => Unit): Unit = xs.foreach(f)

  def ++(other: C): CC[K, V] = factory.safeMk(xs ++ other.xs)

  protected def factory: SafeMapOpsFactory[CC]
}

trait SafeMapOpsFactory[C[_, _]] {
  def empty[K, V]: C[K, V] = safeMk[K, V](Map.empty)

  def apply[K, V](xs: Map[K, V])(implicit n: Numeric[V]): C[K, V] = {
    lazy val invalidPair = xs.find { case (_, v) => !isValid(v) }
    assert(invalidPair.isEmpty, s"Found illegal value in pair: ${invalidPair.get}")
    safeMk(xs)
  }

  protected[collections] def safeMk[K, V](xs: Map[K, V]): C[K, V]
  protected def isValid[V](v: V)(implicit n: Numeric[V]): Boolean
}
