package com.wavesplatform.dex.api

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class ApiMessage(@ApiModelProperty() message: String) extends AnyVal

object ApiMessage {
  implicit val apiMessageFormat: OFormat[ApiMessage] = Json.format
}
