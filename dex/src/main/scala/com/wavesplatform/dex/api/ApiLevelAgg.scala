package com.wavesplatform.dex.api

import com.wavesplatform.dex.model.LevelAgg
import play.api.libs.json.{Format, Json}

case class ApiLevelAgg(amount: Long, price: Long)

object ApiLevelAgg {
  implicit val apiLevelAggFormat: Format[ApiLevelAgg] = Json.format
  def fromLevelAgg(la: LevelAgg): ApiLevelAgg         = ApiLevelAgg(la.amount, la.price)
}
