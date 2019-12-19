package com.wavesplatform.dex.it.assets

import com.wavesplatform.dex.model.MatcherModel.Normalization

trait DoubleOps {
  implicit final class DoubleOpsImplicits(val value: Double) {
    val waves: Long = Normalization.normalizeAmountAndFee(value, 8)
  }
}

object DoubleOps extends DoubleOps
