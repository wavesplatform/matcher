package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.PublicKey
import play.api.libs.json.{Format, Json}

case class ApiMarkets(matcherPublicKey: PublicKey, markets: List[ApiMarket])
object ApiMarkets {
  implicit val apiMarketsFormat: Format[ApiMarkets] = Json.format
}
