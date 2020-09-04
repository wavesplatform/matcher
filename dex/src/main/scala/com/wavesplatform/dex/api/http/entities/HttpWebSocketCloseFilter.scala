package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpWebSocketCloseFilter(@ApiModelProperty() oldest: Int) extends AnyVal

object HttpWebSocketCloseFilter {
  implicit val httpWebSocketCloseFilterFormat: OFormat[HttpWebSocketCloseFilter] = Json.format
}
