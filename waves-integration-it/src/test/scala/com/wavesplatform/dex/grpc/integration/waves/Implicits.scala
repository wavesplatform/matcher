package com.wavesplatform.dex.grpc.integration.waves

import com.wavesplatform.settings.Constants

object Implicits {
  implicit class DoubleExt(val d: Double) extends AnyVal {
    def waves: Long = (BigDecimal(d) * Constants.UnitsInWave).toLong
  }
}
