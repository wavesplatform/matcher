package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.{Json, OFormat}

final case class HttpCalculatedFeeResponse(base: Option[HttpCalculatedFee], discount: Option[HttpCalculatedFee])

object HttpCalculatedFeeResponse {
  implicit val formats: OFormat[HttpCalculatedFeeResponse] = Json.format[HttpCalculatedFeeResponse]
}

final case class HttpCalculatedFee(feeAssetId: Asset, matcherFee: Long)

object HttpCalculatedFee {
  implicit val formats: OFormat[HttpCalculatedFee] = Json.format[HttpCalculatedFee]
}
