package com.wavesplatform.dex.settings

import cats.syntax.apply._
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.ConfigReaderMyOps
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator._
import com.wavesplatform.dex.settings.utils.{ConfigSettingsValidator, rules, validationOf}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
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

  // ====

  implicit val deviationsSettingsReader: ValueReader[DeviationsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateDeviationPercent(settingName: String): ErrorsListOr[Double] = {
      cfgValidator.validateByPredicate[Double](settingName)(_ > 0, "required 0 < percent")
    }

    (
      cfgValidator.validate[Boolean](s"$path.enable"),
      validateDeviationPercent(s"$path.profit"),
      validateDeviationPercent(s"$path.loss"),
      validateDeviationPercent(s"$path.fee")
    ) mapN DeviationsSettings.apply getValueOrThrowErrors
  }
}
