package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json.assetMapFormat
import play.api.libs.json.Format

case class ApiRates(xs: Map[Asset, Double]) extends AnyVal
object ApiRates {
  private val baseFormat = assetMapFormat[Double]
  implicit val rateSettingsFormat: Format[ApiRates] = Format(
    baseFormat.map(apply),
    baseFormat.contramap(_.xs)
  )
}
