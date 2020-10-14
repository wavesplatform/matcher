package com.wavesplatform.dex.settings

import com.wavesplatform.dex.settings.OrderHistorySettings._
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils.{rules, validationOf}
import pureconfig.generic.semiauto

case class OrderHistorySettings(
  enable: Boolean = false,
  ordersBatchLingerMs: Long = defaultBatchLingerMs,
  ordersBatchEntries: Long = defaultBatchEntries,
  eventsBatchLingerMs: Long = defaultBatchLingerMs,
  eventsBatchEntries: Long = defaultBatchEntries
)

object OrderHistorySettings {

  val defaultBatchLingerMs = 1000
  val defaultBatchEntries = 10000

  implicit val orderHistoryConfigReader = semiauto
    .deriveReader[OrderHistorySettings]
    .validatedField(
      validationOf.field[OrderHistorySettings, "ordersBatchLingerMs"].mk(x => rules.gtEq0(x.ordersBatchLingerMs)),
      validationOf.field[OrderHistorySettings, "ordersBatchEntries"].mk(x => rules.gtEq0(x.ordersBatchEntries)),
      validationOf.field[OrderHistorySettings, "eventsBatchLingerMs"].mk(x => rules.gtEq0(x.eventsBatchLingerMs)),
      validationOf.field[OrderHistorySettings, "eventsBatchEntries"].mk(x => rules.gtEq0(x.eventsBatchEntries))
    )

}
