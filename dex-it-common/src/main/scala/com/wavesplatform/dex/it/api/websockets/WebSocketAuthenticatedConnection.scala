package com.wavesplatform.dex.it.api.websockets

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.Message
import akka.stream.Materializer
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.json.Json

class WebSocketAuthenticatedConnection(uri: String)(implicit system: ActorSystem, materializer: Materializer)
    extends WebSocketConnection[WsAddressState](uri, WebSocketAuthenticatedConnection.parseAndLogMessages, true) {

  def getBalancesChanges: Seq[Map[Asset, WsBalances]] = getMessagesBuffer.map(_.balances).distinct
  def getOrderChanges: Seq[WsOrder]                   = getMessagesBuffer.flatMap(_.orders).distinct

  def getSnapshot: WsAddressState = getMessagesBuffer.headOption.getOrElse(WsAddressState.empty)
}

object WebSocketAuthenticatedConnection extends ScorexLogging {
  def parseAndLogMessages(msg: Message): WsAddressState = {
    log.trace(s"Got message: $msg")
    Json.parse(msg.asTextMessage.getStrictText).as[WsAddressState]
  }
}
