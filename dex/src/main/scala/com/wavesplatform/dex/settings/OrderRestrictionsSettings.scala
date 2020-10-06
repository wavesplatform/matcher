package com.wavesplatform.dex.settings

import com.wavesplatform.dex.settings.OrderRestrictionsSettings.Default
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils.{rules, validationOf}
import pureconfig.generic.semiauto

case class OrderRestrictionsSettings(
  stepAmount: Double = Default.stepAmount,
  minAmount: Double = Default.minAmount,
  maxAmount: Double = Default.maxAmount,
  stepPrice: Double = Default.stepPrice,
  minPrice: Double = Default.minPrice,
  maxPrice: Double = Default.maxPrice
)

object OrderRestrictionsSettings {

  val Default =
    OrderRestrictionsSettings(
      stepAmount = 0.00000001,
      minAmount = 0.00000001,
      maxAmount = 1000000000,
      stepPrice = 0.00000001,
      minPrice = 0.00000001,
      maxPrice = 1000000
    )

  implicit val deviationsConfigReader = semiauto
    .deriveReader[OrderRestrictionsSettings]
    .validatedField(
      validationOf.field[OrderRestrictionsSettings, "stepAmount"].mk(x => rules.gt0(x.stepAmount)),
      validationOf.field[OrderRestrictionsSettings, "minAmount"].mk(x => rules.gt0(x.minAmount)),
      validationOf.field[OrderRestrictionsSettings, "maxAmount"].mk(x => rules.gt0(x.maxAmount)),
      validationOf.field[OrderRestrictionsSettings, "stepPrice"].mk(x => rules.gt0(x.stepPrice)),
      validationOf.field[OrderRestrictionsSettings, "minPrice"].mk(x => rules.gt0(x.minPrice)),
      validationOf.field[OrderRestrictionsSettings, "maxPrice"].mk(x => rules.gt0(x.maxPrice)),
      validationOf.field[OrderRestrictionsSettings, "maxAmount"].mk(x => rules.gtN(x.maxAmount, x.minAmount, "min-amount")),
      validationOf.field[OrderRestrictionsSettings, "maxPrice"].mk(x => rules.gtN(x.maxPrice, x.minPrice, "min-price"))
    )

}
