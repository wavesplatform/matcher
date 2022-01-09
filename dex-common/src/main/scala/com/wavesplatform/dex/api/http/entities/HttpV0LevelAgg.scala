package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads}

case class HttpV0LevelAgg(@ApiModelProperty(example = "83187648950") amount: Long, @ApiModelProperty(example = "12079") price: Long)

object HttpV0LevelAgg {
  implicit val httpV0LevelAggReads: Reads[HttpV0LevelAgg] = Json.reads

}
