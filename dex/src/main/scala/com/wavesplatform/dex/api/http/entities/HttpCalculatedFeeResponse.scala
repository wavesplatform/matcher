package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}

@ApiModel(value = "HttpCalculatedFeeResponse")
final case class HttpCalculatedFeeResponse(
  base: Option[HttpCalculatedFeeResponse.CalculatedFee],
  discount: Option[HttpCalculatedFeeResponse.CalculatedFee]
)

object HttpCalculatedFeeResponse {

  @ApiModel(value = "CalculatedFee")
  final case class CalculatedFee(
    @ApiModelProperty(
      name = "feeAssetId",
      value = "Base58 encoded Matcher fee asset ID. Waves is used if field isn't specified",
      dataType = "string",
      example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS",
      required = true
    )
    feeAssetId: Asset,
    matcherFee: Long
  )

  object CalculatedFee {
    implicit val formats: OFormat[CalculatedFee] = Json.format[CalculatedFee]
  }

  implicit val formats: OFormat[HttpCalculatedFeeResponse] = Json.format[HttpCalculatedFeeResponse]
}
