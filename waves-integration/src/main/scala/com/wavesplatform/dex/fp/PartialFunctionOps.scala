package com.wavesplatform.dex.fp

object PartialFunctionOps {
  implicit final class Implicits[A, B](val self: PartialFunction[A, B]) extends AnyVal {
    def toTotal(f: A => B): A => B = self.applyOrElse(_, f)
  }
}
