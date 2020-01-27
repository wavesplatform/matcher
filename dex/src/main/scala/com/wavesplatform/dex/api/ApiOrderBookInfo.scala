package com.wavesplatform.dex.api

import com.wavesplatform.dex.json.stringAsDoubleFormat
import com.wavesplatform.dex.settings.OrderRestrictionsSettings
import play.api.libs.json.{Format, Json, OFormat}

case class ApiOrderBookInfo(restrictions: Option[OrderRestrictionsSettings], matchingRules: ApiOrderBookInfo.MatchingRuleSettings)
object ApiOrderBookInfo {
  implicit val orderBookInfoFormat: OFormat[ApiOrderBookInfo] = Json.format[ApiOrderBookInfo]

  case class MatchingRuleSettings(tickSize: Double)
  object MatchingRuleSettings {
    implicit val matchingRuleSettingsFormat: Format[MatchingRuleSettings] = {
      implicit val doubleFormat: Format[Double] = stringAsDoubleFormat
      Json.format[MatchingRuleSettings]
    }
  }
}
