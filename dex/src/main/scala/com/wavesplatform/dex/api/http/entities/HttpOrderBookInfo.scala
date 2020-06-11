package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpOrderBookInfo(@ApiModelProperty(
                              value = "Restrictions of orders' amount and price",
                              allowEmptyValue = true
                            ) restrictions: Option[HttpOrderRestrictions],
                             @ApiModelProperty(
                              value = "Matching rules, tick size in particular",
                              allowEmptyValue = true
                            )
                            matchingRules: HttpMatchingRules)

object HttpOrderBookInfo {
  implicit val httpOrderBookInfoFormat: OFormat[HttpOrderBookInfo] = Json.format[HttpOrderBookInfo]
}
