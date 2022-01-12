package com.wavesplatform.dex.utils

import com.wavesplatform.dex.tool.LocaleUtils

import java.text.DecimalFormat

object FormatUtils {

  private val format = new DecimalFormat("#.################", LocaleUtils.symbols)

  /** Formats amount or price */
  def formatValue(value: BigDecimal): String = format.format(value.bigDecimal)

}
