package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class HttpSystemStatus(
  @ApiModelProperty() service: String,
  @ApiModelProperty() blockchain: String
)

object HttpSystemStatus {

  implicit val HttpSystemStatusFormat: Format[HttpSystemStatus] = Json.format

  def apply(m: MatcherStatus, b: CombinedStream.Status) = new HttpSystemStatus(m.toString, b.toString)

}
