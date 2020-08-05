package com.wavesplatform.dex.asset

import com.wavesplatform.dex.domain.model.Normalization

trait DoubleOps {

  implicit final class NumericOps[A](val value: A)(implicit num: Numeric[A]) {

    private def normalize(assetDecimals: Int): Long = Normalization.normalizeAmountAndFee(num.toDouble(value), assetDecimals)

    val waves, eth, btc, asset8: Long = normalize(8)
    val usd, wct: Long                = normalize(2)
    val usdn: Long                    = normalize(6)
  }
}

object DoubleOps extends DoubleOps
