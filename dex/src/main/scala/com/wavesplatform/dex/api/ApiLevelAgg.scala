package com.wavesplatform.dex.api

import com.wavesplatform.dex.model.LevelAgg
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ApiLevelAgg(@ApiModelProperty() amount: Long, @ApiModelProperty() price: Long)

object ApiLevelAgg {
  implicit val apiLevelAggFormat: Format[ApiLevelAgg] = Json.format
  def fromLevelAgg(la: LevelAgg): ApiLevelAgg         = ApiLevelAgg(la.amount, la.price)
}
