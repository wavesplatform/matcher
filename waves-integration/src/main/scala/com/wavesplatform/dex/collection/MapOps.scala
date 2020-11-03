package com.wavesplatform.dex.collection

object MapOps {

  // TODO semigroup

  implicit final class Ops2[K1, K2, V](val self: Map[K1, Map[K2, V]]) extends AnyVal {

    def deepReplace(update: Iterable[(K1, Map[K2, V])]): Map[K1, Map[K2, V]] =
      self.deepCombine(update)(_ ++ _)

  }

  implicit final class Ops[K, V](val self: Map[K, V]) extends AnyVal {

    def deepCombine(update: Iterable[(K, V)])(combine: (V, V) => V): Map[K, V] =
      update.foldLeft(self) {
        case (r, (k, v)) =>
          val updatedV = r.get(k).foldLeft(v)((updateV, origV) => combine(origV, updateV))
          r.updated(k, updatedV)
      }

  }

}
