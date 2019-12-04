package com.wavesplatform.dex.it.assets

trait DoubleOps {
  implicit final class DoubleOpsImplicits(val value: Double) {
    val waves: Long = { BigDecimal(value) * Math.pow(10, 8) }.toLong
  }
}

object DoubleOps extends DoubleOps
