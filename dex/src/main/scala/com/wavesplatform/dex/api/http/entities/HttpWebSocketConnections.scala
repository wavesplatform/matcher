package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpWebSocketConnections(@ApiModelProperty() connections: Int) extends AnyVal

object HttpWebSocketConnections {
  implicit val httpWebSocketConnectionsFormat: OFormat[HttpWebSocketConnections] = Json.format
}
