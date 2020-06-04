package com.wavesplatform.dex.api

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class ApiOrderBookInfo(@ApiModelProperty(value = "Restrictions of orders' amount and price") restrictions: Option[ApiOrderRestrictions],
                            @ApiModelProperty(value = "Matching rules, tick size in particular") matchingRules: ApiMatchingRules)

object ApiOrderBookInfo {
  implicit val orderBookInfoFormat: OFormat[ApiOrderBookInfo] = Json.format[ApiOrderBookInfo]
}
