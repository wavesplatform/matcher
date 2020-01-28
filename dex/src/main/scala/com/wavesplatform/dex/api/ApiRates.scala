package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json._
import play.api.libs.json.Format

case class ApiRates(xs: Map[Asset, Double]) extends AnyVal
object ApiRates {
  implicit val rateSettingsFormat: Format[ApiRates] = assetMapFormat[Double].coerce(apply, _.xs)
}
