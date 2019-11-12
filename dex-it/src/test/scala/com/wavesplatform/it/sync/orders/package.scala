package com.wavesplatform.it.sync

import com.wavesplatform.dex.model.MatcherModel.Normalization

package object orders {
  implicit class DoubleOps(value: Double) {
    val waves, eth, btc: Long = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long             = Normalization.normalizePrice(value, 8, 2)
  }

  val percentFee           = 25
  val price                = 0.4.usd
  val fullyAmountWaves     = 150.waves
  val fullyAmountUsd       = 60.usd
  val minimalFee      = 37.5.waves
  val tooLowFee            = 37.49999.waves
  val tooHighFee           = 150.00001.waves
  val partiallyAmountWaves = 75.waves
  val partiallyAmountUsd   = 30.usd
  val partiallyFeeWaves    = 18.75.waves
}
