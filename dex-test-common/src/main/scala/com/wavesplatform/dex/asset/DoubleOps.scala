package com.wavesplatform.dex.asset

import com.wavesplatform.dex.domain.model.Normalization

trait DoubleOps {

  implicit final class NumericOps[A](val value: A)(implicit num: Numeric[A]) {
    val waves, eth, btc, asset8: Long = Normalization.normalizeAmountAndFee(num.toDouble(value), 8)
    val usd, wct: Long                = Normalization.normalizeAmountAndFee(num.toDouble(value), 2)
  }
}

object DoubleOps extends DoubleOps
