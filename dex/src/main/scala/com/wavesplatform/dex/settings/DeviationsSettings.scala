package com.wavesplatform.dex.settings

import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils.{rules, validationOf}
import pureconfig.generic.semiauto

/** Represents market order restrictions. Field values are in percents */
case class DeviationsSettings(enable: Boolean, maxPriceProfit: Double, maxPriceLoss: Double, maxFeeDeviation: Double)

object DeviationsSettings {

  implicit val deviationsConfigReader = semiauto
    .deriveReader[DeviationsSettings]
    .validatedField(
      validationOf.field[DeviationsSettings, "maxPriceProfit"].mk(x => rules.gt0(x.maxPriceProfit)),
      validationOf.field[DeviationsSettings, "maxPriceLoss"].mk(x => rules.gt0(x.maxPriceLoss)),
      validationOf.field[DeviationsSettings, "maxFeeDeviation"].mk(x => rules.gt0(x.maxFeeDeviation))
    )

}
