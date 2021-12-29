package com.wavesplatform.dex

import com.wavesplatform.dex.tool.LocaleUtils

import java.text.DecimalFormat

package object settings {

  private val format = new DecimalFormat("#.################", LocaleUtils.symbols)

  /** Formats amount or price */
  def formatValue(value: BigDecimal): String = format.format(value.bigDecimal)

}
