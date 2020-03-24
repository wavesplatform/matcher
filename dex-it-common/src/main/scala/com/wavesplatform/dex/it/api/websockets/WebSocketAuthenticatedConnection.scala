package com.wavesplatform.dex.it.api.websockets

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.Json

class WebSocketAuthenticatedConnection(uri: String, apiKey: Option[String])(implicit system: ActorSystem, materializer: Materializer)
    extends WebSocketConnection[WsAddressState](uri, msg => Json.parse(msg.asTextMessage.getStrictText).as[WsAddressState], true, apiKey) {

  def getBalancesChanges: Seq[Map[Asset, WsBalances]] = getMessagesBuffer.map(_.balances).distinct
  def getOrderChanges: Seq[WsOrder]                   = getMessagesBuffer.flatMap(_.orders).distinct

  def getSnapshot: WsAddressState = getMessagesBuffer.headOption.getOrElse(WsAddressState.empty)
}
