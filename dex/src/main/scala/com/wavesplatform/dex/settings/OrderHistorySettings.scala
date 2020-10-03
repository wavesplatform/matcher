package com.wavesplatform.dex.settings

import cats.syntax.apply._
import com.wavesplatform.dex.settings.OrderHistorySettings._
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.ConfigReaderMyOps
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator._
import com.wavesplatform.dex.settings.utils.{ConfigSettingsValidator, rules, validationOf}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import pureconfig.generic.semiauto

case class OrderHistorySettings(
    ordersBatchLingerMs: Long = defaultBatchLingerMs,
    ordersBatchEntries: Long = defaultBatchEntries,
    eventsBatchLingerMs: Long = defaultBatchLingerMs,
    eventsBatchEntries: Long = defaultBatchEntries
)

object OrderHistorySettings {

  val defaultBatchLingerMs = 1000
  val defaultBatchEntries  = 10000

  implicit val orderHistoryConfigReader = semiauto
    .deriveReader[OrderHistorySettings]
    .validatedField(
      validationOf.field[OrderHistorySettings, "ordersBatchLingerMs"].mk(x => rules.gtEq0(x.ordersBatchLingerMs)),
      validationOf.field[OrderHistorySettings, "ordersBatchEntries"].mk(x => rules.gtEq0(x.ordersBatchEntries)),
      validationOf.field[OrderHistorySettings, "eventsBatchLingerMs"].mk(x => rules.gtEq0(x.eventsBatchLingerMs)),
      validationOf.field[OrderHistorySettings, "eventsBatchEntries"].mk(x => rules.gtEq0(x.eventsBatchEntries))
    )

  // ====

  implicit val orderHistorySettingsReader: ValueReader[Option[OrderHistorySettings]] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateBatchSettings(settingName: String, defaultValue: Long): ErrorsListOr[Long] =
      cfgValidator.validateByPredicateWithDefault(s"$path.$settingName")(_ >= 0, s"required 0 <= ${settingName.replace("-", " ")}", defaultValue)

    if (cfgValidator.validateWithDefault(s"$path.enabled", false) getValueOrThrowErrors) {
      Some(
        (
          validateBatchSettings("orders-batch-linger-ms", defaultBatchLingerMs),
          validateBatchSettings("orders-batch-entries", defaultBatchEntries),
          validateBatchSettings("events-batch-linger-ms", defaultBatchLingerMs),
          validateBatchSettings("events-batch-entries", defaultBatchEntries)
        ) mapN OrderHistorySettings.apply getValueOrThrowErrors
      )
    } else None
  }
}
