package com.wavesplatform.dex.doc

import shapeless._

trait Documented[T] {
  def doc(x: T): String
}

class Documentation[Base] {
  trait BaseDocumented[A <: Coproduct] { def descendantsDoc: List[String] }

  implicit def caseCNil: BaseDocumented[CNil] = new BaseDocumented[CNil] {
    def descendantsDoc: List[String] = Nil
  }

  implicit def caseCCons[A <: Base, T <: Coproduct](implicit rec: BaseDocumented[T], ct: Sample[A], doc: Documented[Base]): BaseDocumented[A :+: T] =
    new BaseDocumented[A :+: T] {
      def descendantsDoc: List[String] = doc.doc(ct.sample) :: rec.descendantsDoc
    }

  def mk[C <: Coproduct](implicit g: Generic.Aux[Base, C], s: BaseDocumented[C]): List[String] = s.descendantsDoc
}
