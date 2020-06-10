package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._

case class ApiMatcherPublicSettings(@ApiModelProperty(
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
                                      dataType = "List[string]",
                                    ) priceAssets: Seq[Asset],
                                    @ApiModelProperty(
                                      value = "Order Fee Mode, possible modes: FeeModeDynamic, FeeModeFixed, FeeModePercent"
                                    ) orderFee: ApiOrderFeeMode,
                                    @ApiModelProperty(
                                      value = "List of supported order versions",
                                      dataType = "List[integer]"
                                    ) orderVersions: Seq[Byte],
                                    @ApiModelProperty(
                                      value = "Network byte",
                                      dataType = "integer",
                                      example = "89"
                                    ) networkByte: Int)

object ApiMatcherPublicSettings {
  implicit val matcherPublicSettingsFormat: OFormat[ApiMatcherPublicSettings] = Json.format[ApiMatcherPublicSettings]
}
