package com.wavesplatform.dex.settings

import com.wavesplatform.dex.settings.SubscriptionsSettings.default
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils.{rules, validationOf}
import pureconfig.generic.semiauto

final case class SubscriptionsSettings(maxOrderBookNumber: Int = default.maxOrderBookNumber, maxAddressNumber: Int = default.maxAddressNumber)

object SubscriptionsSettings {

  val default = SubscriptionsSettings(10, 10)

  implicit val subscriptionsConfigReader = semiauto
    .deriveReader[SubscriptionsSettings]
    .validatedField(
      validationOf.field[SubscriptionsSettings, "maxOrderBookNumber"].mk(x => rules.gtN(x.maxOrderBookNumber, 0)),
      validationOf.field[SubscriptionsSettings, "maxAddressNumber"].mk(x => rules.gtN(x.maxAddressNumber, 0))
    )
}
