package com.wavesplatform.it.dex

import com.wavesplatform.dex.AssetPairDecimals
import AD._

object AD {
  val ethWavesPairDecimals = AssetPairDecimals(8, 8)
  val wavesUsdPairDecimals = AssetPairDecimals(8, 2)
}

trait DoubleOps {
  implicit final class DoubleOpsImplicits(val value: Double) {
    val waves: Long = wavesUsdPairDecimals.amount(value)
    val usd: Long   = wavesUsdPairDecimals.price(value)
    val eth: Long   = ethWavesPairDecimals.amount(value)
  }
}

object DoubleOps extends DoubleOps
