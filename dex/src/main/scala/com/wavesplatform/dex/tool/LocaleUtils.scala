package com.wavesplatform.dex.tool

import java.text.DecimalFormatSymbols
import java.util.{Locale => JLocale}

object LocaleUtils {

  val symbols = new DecimalFormatSymbols(JLocale.US)
}
