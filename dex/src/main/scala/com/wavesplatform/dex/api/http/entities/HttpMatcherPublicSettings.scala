package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._

case class HttpMatcherPublicSettings(
  @ApiModelProperty(
    value = "Base58 encoded Matcher public key",
    dataType = "string",
    example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8"
  ) matcherPublicKey: PublicKey,
  @ApiModelProperty(
    value = "Current Matcher version",
    example = "2.1.3.5"
  ) matcherVersion: String,
  @ApiModelProperty(
    value = "List of the Base58 encoded price asset IDs",
    dataType = "List[string]"
  ) priceAssets: Seq[Asset],
  @ApiModelProperty(
    value = "Asset Rates as Map[Base58 encoded Asset ID, Long]",
    dataType = "Map[string,number]"
  ) rates: Map[Asset, Double],
  @ApiModelProperty(
    value = "Order Fee Mode, possible modes: FeeModeDynamic, FeeModeFixed, FeeModePercent"
  ) orderFee: HttpOrderFeeMode,
  @ApiModelProperty(
    value = "List of supported order versions",
    dataType = "List[integer]"
  ) orderVersions: Seq[Byte],
  @ApiModelProperty(
    value = "Network byte",
    dataType = "integer",
    example = "89"
  ) networkByte: Int
)

object HttpMatcherPublicSettings {
  implicit val httpMatcherPublicSettingsFormat: OFormat[HttpMatcherPublicSettings] = Json.format[HttpMatcherPublicSettings]
}
