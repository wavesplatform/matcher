package com.wavesplatform.dex.it.assets

import com.wavesplatform.dex.model.MatcherModel.Normalization

trait DoubleOps {
  implicit final class DoubleOpsImplicits(val value: Double) {
    val waves, eth, btc: Long = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long             = Normalization.normalizePrice(value, 8, 2)
  }
}

object DoubleOps extends DoubleOps
