package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpMarketDataWithMeta(
  @ApiModelProperty(
    value = "Base58 encoded amount asset ID",
    dataType = "string",
    example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS",
    required = true
  ) amountAsset: Asset,
  @ApiModelProperty(example = "BTC", required = true)
  amountAssetName: String,
  @ApiModelProperty(value = "Info about amount asset decimals", required = true)
  amountAssetInfo: Option[HttpAssetInfo],
  @ApiModelProperty(
    value = "Base58 encoded price asset ID",
    dataType = "string",
    example = "34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ",
    required = true
  ) priceAsset: Asset,
  @ApiModelProperty(example = "USDT", required = true) priceAssetName: String,
  @ApiModelProperty(value = "Info about price asset decimals") priceAssetInfo: Option[HttpAssetInfo],
  @ApiModelProperty(required = true) created: Long,
  @ApiModelProperty() restrictions: Option[HttpOrderRestrictions],
  @ApiModelProperty(required = true) matchingRules: HttpMatchingRules
)

object HttpMarketDataWithMeta {
  implicit val httpMarketDataWithMetaFormat: OFormat[HttpMarketDataWithMeta] = Json.format[HttpMarketDataWithMeta]
}
