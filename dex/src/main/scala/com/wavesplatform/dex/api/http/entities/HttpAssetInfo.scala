package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.actors.MatcherActor.AssetInfo
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class HttpAssetInfo(@ApiModelProperty(example = "8") decimals: Int)

object HttpAssetInfo {
  implicit val httpAssetInfoFormat: Format[HttpAssetInfo] = Json.format[HttpAssetInfo]
  def fromAssetInfo(ai: AssetInfo): HttpAssetInfo = HttpAssetInfo(ai.decimals)
}
