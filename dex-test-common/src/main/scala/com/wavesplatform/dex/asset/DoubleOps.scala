package com.wavesplatform.dex.asset

import com.wavesplatform.dex.domain.model.Normalization

trait DoubleOps {
  implicit final class DoubleOpsImplicits(val value: Double) {
    val waves, eth, btc: Long = Normalization.normalizeAmountAndFee(value, 8)
    val usd, wct: Long        = Normalization.normalizeAmountAndFee(value, 2)
  }
}

object DoubleOps extends DoubleOps
