package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpMessage(@ApiModelProperty() message: String) extends AnyVal

object HttpMessage {
  implicit val httpMessageFormat: OFormat[HttpMessage] = Json.format
}
