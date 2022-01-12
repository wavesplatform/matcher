package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.statuses.{CombinedStreamStatus, MatcherStatus}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class HttpSystemStatus(
  @ApiModelProperty() service: MatcherStatus,
  @ApiModelProperty() blockchain: CombinedStreamStatus
)

object HttpSystemStatus {

  implicit val HttpSystemStatusFormat: Format[HttpSystemStatus] = Json.format

  def apply(service: MatcherStatus, blockchain: CombinedStreamStatus) = new HttpSystemStatus(service, blockchain)

}
