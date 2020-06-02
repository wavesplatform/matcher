package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.PublicKey
import play.api.libs.json.{Json, OFormat}

case class ApiTradingMarkets(matcherPublicKey: PublicKey, markets: Seq[ApiMarketDataWithMeta])

object ApiTradingMarkets {
  implicit val apiTradingMarketsFormat: OFormat[ApiTradingMarkets] = Json.format[ApiTradingMarkets]
}
