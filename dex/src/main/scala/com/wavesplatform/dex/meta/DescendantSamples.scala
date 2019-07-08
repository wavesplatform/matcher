package com.wavesplatform.dex.meta

import shapeless._

class DescendantSamples[Base] {
  trait Helper[A <: Coproduct] { def descendants: List[Base] }

  implicit def caseCNil: Helper[CNil] = new Helper[CNil] {
    def descendants: List[Base] = Nil
  }

  implicit def caseCCons[A <: Base, T <: Coproduct](implicit rec: Helper[T], ct: Sample[A]): Helper[A :+: T] =
    new Helper[A :+: T] {
      def descendants: List[Base] = ct.sample :: rec.descendants
    }

  def run[C <: Coproduct](implicit g: Generic.Aux[Base, C], s: Helper[C]): List[Base] = s.descendants
}
