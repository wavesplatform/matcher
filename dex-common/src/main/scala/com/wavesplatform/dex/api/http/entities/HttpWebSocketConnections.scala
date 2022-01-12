package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpWebSocketConnections(@ApiModelProperty() connections: Int, @ApiModelProperty() clientAndOs: Map[String, Int])

object HttpWebSocketConnections {
  implicit val httpWebSocketConnectionsFormat: OFormat[HttpWebSocketConnections] = Json.format
}
