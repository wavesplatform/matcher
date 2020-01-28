package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json._
import play.api.libs.json.Format

case class ApiBalance(xs: Map[Asset, Long]) extends AnyVal
object ApiBalance {
  implicit val apiBalanceFormat: Format[ApiBalance] = assetMapFormat[Long].coerce(apply, _.xs)
}
