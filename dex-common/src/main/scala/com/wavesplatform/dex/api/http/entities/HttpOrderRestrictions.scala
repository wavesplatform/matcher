package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.utils.JsonImplicits
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, OFormat}

case class HttpOrderRestrictions(
  @ApiModelProperty(dataType = "string", example = "0.001") stepAmount: Double,
  @ApiModelProperty(dataType = "string", example = "1000000") minAmount: Double,
  @ApiModelProperty(dataType = "string", example = "0.00000001") maxAmount: Double,
  @ApiModelProperty(dataType = "string", example = "0.001") stepPrice: Double,
  @ApiModelProperty(dataType = "string", example = "100000") minPrice: Double,
  @ApiModelProperty(dataType = "string", example = "0.00000001") maxPrice: Double
)

object HttpOrderRestrictions {

  implicit val doubleFormat: Format[Double] = JsonImplicits.stringAsDoubleFormat
  implicit val httpOrderRestrictionsFormat: OFormat[HttpOrderRestrictions] = Json.format[HttpOrderRestrictions]

}
