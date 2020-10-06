package com.wavesplatform.dex.settings.utils

import java.text.DecimalFormat

import cats.implicits.{catsSyntaxOptionId, none}

object rules {
  private val doubleFormat = new DecimalFormat("#")
  doubleFormat.setMinimumIntegerDigits(1)
  doubleFormat.setMaximumFractionDigits(20)

  def gtEq0[T: Numeric](x: T): Option[String] = gtEqN(x, Numeric[T].zero)
  def gtEqN[T: Numeric](x: T, n: T): Option[String] = if (Numeric[T].gteq(x, n)) none else s"$x should be >= $n".some

  def gt0[T: Numeric](x: T): Option[String] = gtN(x, Numeric[T].zero)
  def gtN[T: Numeric](x: T, n: T): Option[String] = if (Numeric[T].gt(x, n)) none else s"$x should be > $n".some

  def gtN(x: BigDecimal, n: BigDecimal, nField: String): Option[String] =
    if (x > n) none else s"${x.bigDecimal.toPlainString} should be > $nField: ${n.bigDecimal.toPlainString}".some

  def gtN(x: Double, n: Double, nConfigField: String): Option[String] =
    if (x > n) none else s"${doubleFormat.format(x)} should be > $nConfigField: ${doubleFormat.format(n)}".some

  def gtN[T: Numeric](x: T, n: T, nConfigField: String): Option[String] =
    if (Numeric[T].gt(x, n)) none else s"$x should be > $nConfigField: $n".some

}
