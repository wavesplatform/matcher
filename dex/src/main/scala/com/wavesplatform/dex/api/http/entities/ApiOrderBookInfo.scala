package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class ApiOrderBookInfo(@ApiModelProperty(
                              value = "Restrictions of orders' amount and price",
                              allowEmptyValue = true
                            ) restrictions: Option[ApiOrderRestrictions],
                            @ApiModelProperty(
                              value = "Matching rules, tick size in particular",
                              allowEmptyValue = true
                            )
                            matchingRules: ApiMatchingRules)

object ApiOrderBookInfo {
  implicit val orderBookInfoFormat: OFormat[ApiOrderBookInfo] = Json.format[ApiOrderBookInfo]
}
