package com.wavesplatform.dex.it.assets

import com.wavesplatform.dex.it.assets.AD._
import scala.math.BigDecimal.RoundingMode.CEILING

// TODO remove duplicate
case class AssetPairDecimals(amountDecimals: Byte, priceDecimals: Byte) {
  def amount(a: Double): Long         = { BigDecimal(a) * Math.pow(10, amountDecimals) }.toLong
  def price(p: Double): Long          = { BigDecimal(p) * Math.pow(10, 8 + priceDecimals - amountDecimals) }.toLong
  def minAmountFor(price: Long): Long = { BigDecimal(Math.pow(10, amountDecimals)) / BigDecimal(price) }.setScale(0, CEILING).toLong
}

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
