package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpWebSocketDropFilter(@ApiModelProperty() oldest: Int) extends AnyVal

object HttpWebSocketDropFilter {
  implicit val httpWebSocketDropFilterFormat: OFormat[HttpWebSocketDropFilter] = Json.format
}
