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
      validationOf.field[SubscriptionsSettings, "maxOrderBookNumber"].mk(x => rules.gt0(x.maxOrderBookNumber)),
      validationOf.field[SubscriptionsSettings, "maxAddressNumber"].mk(x => rules.gt0(x.maxAddressNumber))
    )

}
