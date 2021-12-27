package com.wavesplatform.dex.collections

// TODO DEX-994
object MapOps {

  implicit final class Ops[K, V](val self: Map[K, V]) extends AnyVal {

    def deepCombine(update: Iterable[(K, V)])(combine: (V, V) => V): Map[K, V] =
      update.foldLeft(self) {
        case (r, (k, updateV)) =>
          val updatedV = r.get(k).fold(updateV)(combine(_, updateV))
          r.updated(k, updatedV)
      }

  }

  implicit final class Ops2D[K1, K2, V](val self: Map[K1, Map[K2, V]]) extends AnyVal {
    def deepReplace(update: Iterable[(K1, Map[K2, V])]): Map[K1, Map[K2, V]] = self.deepCombine(update)(_ ++ _)
  }

}
