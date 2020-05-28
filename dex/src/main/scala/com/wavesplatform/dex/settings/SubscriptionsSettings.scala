package com.wavesplatform.dex.settings

import cats.syntax.apply._
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

final case class SubscriptionsSettings(maxOrderBookNumber: Int, maxAddressNumber: Int)

object SubscriptionsSettings {

  val default: SubscriptionsSettings = SubscriptionsSettings(10, 10)

  implicit val subscriptionSettingsReader: ValueReader[SubscriptionsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)
    (
      cfgValidator.validateByPredicate[Int](s"$path.max-order-book-number")(_ > 1, "max order book number should be > 1"),
      cfgValidator.validateByPredicate[Int](s"$path.max-address-number")(_ > 1, "max address number should be > 1"),
    ) mapN SubscriptionsSettings.apply getValueOrThrowErrors
  }
}
