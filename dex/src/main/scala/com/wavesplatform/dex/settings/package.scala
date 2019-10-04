package com.wavesplatform.dex

package object settings {

  private val format = new java.text.DecimalFormat("#.########")

  /** Formats amount or price */
  def formatValue(value: Double): String = format.format(value)
}
