package com.wavesplatform.dex.settings

import com.wavesplatform.dex.settings.utils.ConfigReaderOps.ConfigReaderMyOps
import com.wavesplatform.dex.settings.utils.{rules, validationOf}
import pureconfig.generic.semiauto

/** Represents market order restrictions. Field values are in percents */
case class DeviationsSettings(enable: Boolean, profit: Double, loss: Double, fee: Double)
// TODO: maxPriceProfit, maxPriceLoss, maxFeeDeviation

object DeviationsSettings {

  implicit val deviationsConfigReader = semiauto
    .deriveReader[DeviationsSettings]
    .validatedField(
      validationOf.field[DeviationsSettings, "profit"].mk(x => rules.gt0(x.profit)),
      validationOf.field[DeviationsSettings, "loss"].mk(x => rules.gt0(x.loss)),
      validationOf.field[DeviationsSettings, "fee"].mk(x => rules.gt0(x.fee))
    )
}
