package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class ApiMarketDataWithMeta(
    @ApiModelProperty(
      value = "Base58 encoded amount asset ID",
      dataType = "string",
      example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
    ) amountAsset: Asset,
    @ApiModelProperty(example = "BTC") amountAssetName: String,
    @ApiModelProperty(value = "Info about amount asset decimals", allowEmptyValue = true) amountAssetInfo: Option[ApiAssetInfo],
    @ApiModelProperty(
      value = "Base58 encoded price asset ID",
      dataType = "string",
      example = "34N9YcEETLWn93qYQ64EsP1x89tSruJU44RrEMSXXEPJ"
    ) priceAsset: Asset,
    @ApiModelProperty(example = "USDT") priceAssetName: String,
    @ApiModelProperty(value = "Info about price asset decimals", allowEmptyValue = true) priceAssetInfo: Option[ApiAssetInfo],
    @ApiModelProperty() created: Long,
    @ApiModelProperty(allowEmptyValue = true) restrictions: Option[ApiOrderRestrictions],
    @ApiModelProperty() matchingRules: ApiMatchingRules)

object ApiMarketDataWithMeta {
  implicit val apiMarketDataFormat: OFormat[ApiMarketDataWithMeta] = Json.format[ApiMarketDataWithMeta]
}
