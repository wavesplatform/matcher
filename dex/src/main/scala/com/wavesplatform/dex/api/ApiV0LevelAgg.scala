package com.wavesplatform.dex.api

import com.wavesplatform.dex.model.LevelAgg
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads}

case class ApiV0LevelAgg(@ApiModelProperty() amount: Long, @ApiModelProperty() price: Long)

object ApiV0LevelAgg {
  implicit val apiV0LevelAggReads: Reads[ApiV0LevelAgg] = Json.reads
  def fromLevelAgg(la: LevelAgg): ApiV0LevelAgg         = ApiV0LevelAgg(la.amount, la.price)
}
