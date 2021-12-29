package com.wavesplatform.dex.it.api.responses.node

import play.api.libs.json.{Format, Json}

case class WavesBalanceResponse(address: String, confirmations: Int, balance: Long)

object WavesBalanceResponse {
  implicit val balanceFormat: Format[WavesBalanceResponse] = Json.format
}
