package com.wavesplatform.dex.api

import com.wavesplatform.dex.json
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, OFormat}

case class ApiMatchingRules(@ApiModelProperty(dataType = "string", example = "0.0001") tickSize: Double)

object ApiMatchingRules {
  implicit val doubleFormat: Format[Double]                      = json.stringAsDoubleFormat
  implicit val apiMatchingRulesFormat: OFormat[ApiMatchingRules] = Json.format[ApiMatchingRules]
}
