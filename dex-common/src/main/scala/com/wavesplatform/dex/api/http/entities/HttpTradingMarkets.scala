package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.account.PublicKey
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpTradingMarkets(
  @ApiModelProperty(
    value = "Base58 encoded Matcher Public Key",
    dataType = "string",
    example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8"
  ) matcherPublicKey: PublicKey,
  @ApiModelProperty(
    value = "Market data with meta information",
    dataType = "[Lcom.wavesplatform.dex.api.http.entities.HttpMarketDataWithMeta;"
  ) markets: Seq[HttpMarketDataWithMeta]
)

object HttpTradingMarkets {
  implicit val httpTradingMarketsFormat: OFormat[HttpTradingMarkets] = Json.format[HttpTradingMarkets]
}
