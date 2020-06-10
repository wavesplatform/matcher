package com.wavesplatform.dex.api

import com.wavesplatform.dex.market.MatcherActor.AssetInfo
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ApiAssetInfo(@ApiModelProperty(example = "8") decimals: Int)

object ApiAssetInfo {
  implicit val apiAssetInfoFormat: Format[ApiAssetInfo] = Json.format[ApiAssetInfo]
  def fromAssetInfo(ai: AssetInfo): ApiAssetInfo        = ApiAssetInfo(ai.decimals)
}
