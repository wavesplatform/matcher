package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class HttpSystemStatus(
  @ApiModelProperty() service: MatcherStatus,
  @ApiModelProperty() blockchain: CombinedStream.Status
)

object HttpSystemStatus {

  implicit val HttpSystemStatusFormat: Format[HttpSystemStatus] = Json.format

  def apply(service: MatcherStatus, blockchain: CombinedStream.Status) = new HttpSystemStatus(service, blockchain)

}
