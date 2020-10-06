package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.json
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, OFormat}

case class HttpMatchingRules(@ApiModelProperty(dataType = "string", example = "0.0001") tickSize: Double)

object HttpMatchingRules {
  implicit val doubleFormat: Format[Double] = json.stringAsDoubleFormat
  implicit val httpMatchingRulesFormat: OFormat[HttpMatchingRules] = Json.format[HttpMatchingRules]
}
