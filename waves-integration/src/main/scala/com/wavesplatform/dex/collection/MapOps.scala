package com.wavesplatform.dex.collection

object MapOps {

  implicit final class Ops[K1, K2, V](val self: Map[K1, Map[K2, V]]) extends AnyVal {

    def deepReplace(update: Map[K1, Map[K2, V]]): Map[K1, Map[K2, V]] =
      update.foldLeft(self) {
        case (r, (k2, xs)) =>
          val orig = r.getOrElse(k2, Map.empty)
          r.updated(k2, orig ++ xs)
      }

  }

}
