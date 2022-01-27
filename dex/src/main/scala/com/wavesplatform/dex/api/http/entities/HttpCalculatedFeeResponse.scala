package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.{Json, OFormat}

final case class HttpCalculatedFeeResponse(
  base: Option[HttpCalculatedFeeResponse.CalculatedFee],
  discount: Option[HttpCalculatedFeeResponse.CalculatedFee]
)

object HttpCalculatedFeeResponse {

  final case class CalculatedFee(feeAssetId: Asset, matcherFee: Long)

  object CalculatedFee {
    implicit val formats: OFormat[CalculatedFee] = Json.format[CalculatedFee]
  }

  implicit val formats: OFormat[HttpCalculatedFeeResponse] = Json.format[HttpCalculatedFeeResponse]
}
