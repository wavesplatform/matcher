package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.json.assetPairMapFormat
import play.api.libs.json.Format

case class ApiSnapshotOffsets(xs: Map[AssetPair, Long]) extends AnyVal
object ApiSnapshotOffsets {
  private val baseFormat = assetPairMapFormat[Long]
  implicit val rateSettingsFormat: Format[ApiSnapshotOffsets] = Format(
    baseFormat.map(apply),
    baseFormat.contramap(_.xs)
  )
}
