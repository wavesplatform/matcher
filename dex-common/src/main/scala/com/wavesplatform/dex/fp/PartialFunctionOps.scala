package com.wavesplatform.dex.fp

object PartialFunctionOps {

  implicit final class Implicits[A, B](val self: PartialFunction[A, B]) extends AnyVal {
    def toTotal(f: A => B): A => B = self.applyOrElse(_, f)
  }

  // Helps scalac to determine types
  def mkPartial[A, B](f: PartialFunction[A, B]): PartialFunction[A, B] = f
}
