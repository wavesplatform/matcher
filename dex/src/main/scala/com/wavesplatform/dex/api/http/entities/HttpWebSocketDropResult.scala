package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpWebSocketDropResult(@ApiModelProperty() droppedConnections: Int) extends AnyVal

object HttpWebSocketDropResult {
  implicit val httpWebSocketDropResultFormat: OFormat[HttpWebSocketDropResult] = Json.format
}
