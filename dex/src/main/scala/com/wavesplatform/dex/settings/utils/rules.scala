package com.wavesplatform.dex.settings.utils

import cats.implicits.{catsSyntaxOptionId, none}

object rules {
  def gtEq0[T: Numeric](x: T): Option[String]       = gtEqN(x, Numeric[T].zero)
  def gtEqN[T: Numeric](x: T, n: T): Option[String] = if (Numeric[T].gteq(x, n)) none else s"$x should be >= $n".some

  def gt0[T: Numeric](x: T): Option[String]       = gtN(x, Numeric[T].zero)
  def gtN[T: Numeric](x: T, n: T): Option[String] = if (Numeric[T].gt(x, n)) none else s"$x should be > $n".some
}
