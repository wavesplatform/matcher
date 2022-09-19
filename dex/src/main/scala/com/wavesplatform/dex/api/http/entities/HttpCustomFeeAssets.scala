package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset._
import play.api.libs.json.{Json, OFormat}

final case class HttpCustomFeeAssets(assets: Set[Asset])

object HttpCustomFeeAssets {
  implicit val httpCustomFeeAssetsFormat: OFormat[HttpCustomFeeAssets] = Json.format
}
