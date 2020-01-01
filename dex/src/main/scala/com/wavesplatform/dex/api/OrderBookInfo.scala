package com.wavesplatform.dex.api

import com.wavesplatform.dex.json.stringAsDoubleFormat
import com.wavesplatform.dex.settings.OrderRestrictionsSettings
import play.api.libs.json.{Format, Json, OFormat}

case class OrderBookInfo(restrictions: Option[OrderRestrictionsSettings], matchingRules: OrderBookInfo.MatchingRuleSettings)
object OrderBookInfo {
  implicit val orderBookInfoFormat: OFormat[OrderBookInfo] = Json.format[OrderBookInfo]

  case class MatchingRuleSettings(tickSize: Double)
  object MatchingRuleSettings {
    implicit val matchingRuleSettingsFormat: Format[MatchingRuleSettings] = {
      implicit val doubleFormat: Format[Double] = stringAsDoubleFormat
      Json.format[MatchingRuleSettings]
    }
  }
}
