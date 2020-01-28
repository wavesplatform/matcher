package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.json._
import play.api.libs.json.Format

case class ApiSnapshotOffsets(xs: Map[AssetPair, Long]) extends AnyVal
object ApiSnapshotOffsets {
  implicit val rateSettingsFormat: Format[ApiSnapshotOffsets] = assetPairMapFormat[Long].coerce(apply, _.xs)
}
