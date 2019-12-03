package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.{ErrorsListOr, _}
import monix.eval.Coeval
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{JsObject, Json}

case class OrderRestrictionsSettings(stepAmount: Double,
                                     minAmount: Double,
                                     maxAmount: Double,
                                     stepPrice: Double,
                                     minPrice: Double,
                                     maxPrice: Double) {

  val getJson: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj(
      "stepAmount" -> formatValue(stepAmount),
      "minAmount"  -> formatValue(minAmount),
      "maxAmount"  -> formatValue(maxAmount),
      "stepPrice"  -> formatValue(stepPrice),
      "minPrice"   -> formatValue(minPrice),
      "maxPrice"   -> formatValue(maxPrice)
    )
  }
}

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

  implicit val orderRestrictionsSettingsReader: ValueReader[OrderRestrictionsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateSizeMinMax(stepSettingName: String,
                           minSettingName: String,
                           maxSettingName: String,
                           stepDefaultValue: Double,
                           minDefaultValue: Double,
                           maxDefaultValue: Double): ErrorsListOr[(Double, Double, Double)] = {

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
      validateSizeMinMax(s"$path.step-price", s"$path.min-price", s"$path.max-price", Default.stepPrice, Default.minPrice, Default.maxPrice),
    ) mapN {
      case ((stepAmount, minAmount, maxAmount), (stepPrice, minPrice, maxPrice)) =>
        OrderRestrictionsSettings(stepAmount, minAmount, maxAmount, stepPrice, minPrice, maxPrice)
    } getValueOrThrowErrors
  }
}
