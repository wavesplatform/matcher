package com.wavesplatform.dex

import com.typesafe.config.Config

package object settings {

  implicit def toConfigOps(config: Config): ConfigExtraOps = new ConfigExtraOps(config)

  private val format = new java.text.DecimalFormat("#.################")

  /** Formats amount or price */
  def formatValue(value: Double): String = format.format(value)
}
