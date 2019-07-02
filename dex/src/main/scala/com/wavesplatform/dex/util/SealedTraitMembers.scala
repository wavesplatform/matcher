package com.wavesplatform.dex.util

import shapeless._

class SealedTraitMembers[A] {
  def apply[C <: Coproduct, K <: HList]()(implicit
                                          gen: LabelledGeneric.Aux[A, C],
                                          keys: ops.union.Keys.Aux[C, K],
                                          toSet: ops.hlist.ToTraversable.Aux[K, Set, Symbol]): Set[String] = toSet(keys()).map(_.name)
}

object SealedTraitMembers {
  def of[A] = new SealedTraitMembers[A]
}
