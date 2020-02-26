package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.{Format, Json, Reads}

case class WsAddressState(balances: Map[Asset, WsBalances])

object WsAddressState {
  implicit val balancesReads: Reads[Map[Asset, WsBalances]] = Reads.seq[(Asset, WsBalances)].map(_.toMap)
  implicit val format: Format[WsAddressState]               = Json.format
}
