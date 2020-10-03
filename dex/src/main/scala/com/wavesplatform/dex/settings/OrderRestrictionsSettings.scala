package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.syntax.apply._
import com.wavesplatform.dex.settings.OrderRestrictionsSettings.Default
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.ConfigReaderMyOps
import com.wavesplatform.dex.settings.utils.{ConfigSettingsValidator, rules, validationOf}
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator.{ErrorsListOr, _}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
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
      validationOf.field[OrderRestrictionsSettings, "maxAmount"].mk(x => rules.gtN(x.maxAmount, x.minAmount, "minAmount")),
      validationOf.field[OrderRestrictionsSettings, "maxPrice"].mk(x => rules.gtN(x.maxPrice, x.minPrice, "minPrice"))
    )

  // ====

  implicit val orderRestrictionsSettingsReader: ValueReader[OrderRestrictionsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateSizeMinMax(
        stepSettingName: String,
        minSettingName: String,
        maxSettingName: String,
        stepDefaultValue: Double,
        minDefaultValue: Double,
        maxDefaultValue: Double
    ): ErrorsListOr[(Double, Double, Double)] = {

      def validateSetting(settingName: String, defaultValue: Double): ErrorsListOr[Double] =
        cfgValidator.validateByPredicateWithDefault[Double](settingName)(_ > 0, s"required 0 < value", defaultValue)

      (
        validateSetting(stepSettingName, stepDefaultValue),
        validateSetting(minSettingName, minDefaultValue),
        validateSetting(maxSettingName, maxDefaultValue)
      ).mapN(Tuple3.apply)
        .ensure(NonEmptyList(s"Required $minSettingName < $maxSettingName", Nil)) { case (_, min, max) => min < max }
    }

    (
      validateSizeMinMax(s"$path.step-amount", s"$path.min-amount", s"$path.max-amount", Default.stepAmount, Default.minAmount, Default.maxAmount),
      validateSizeMinMax(s"$path.step-price", s"$path.min-price", s"$path.max-price", Default.stepPrice, Default.minPrice, Default.maxPrice)
    ) mapN { case ((stepAmount, minAmount, maxAmount), (stepPrice, minPrice, maxPrice)) =>
      OrderRestrictionsSettings(stepAmount, minAmount, maxAmount, stepPrice, minPrice, maxPrice)
    } getValueOrThrowErrors
  }
}
