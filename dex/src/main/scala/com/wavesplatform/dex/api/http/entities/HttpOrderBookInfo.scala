package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpOrderBookInfo(@ApiModelProperty(value = "Restrictions of orders' amount and price")
                             restrictions: Option[HttpOrderRestrictions],
                             @ApiModelProperty(value = "Matching rules, tick size in particular", required = true)
                             matchingRules: HttpMatchingRules)

object HttpOrderBookInfo {
  implicit val httpOrderBookInfoFormat: OFormat[HttpOrderBookInfo] = Json.format[HttpOrderBookInfo]
}
