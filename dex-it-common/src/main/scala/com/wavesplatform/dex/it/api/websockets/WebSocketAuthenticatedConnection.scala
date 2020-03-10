package com.wavesplatform.dex.it.api.websockets

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.Json

class WebSocketAuthenticatedConnection(uri: String)(implicit system: ActorSystem, materializer: Materializer)
    extends WebSocketConnection[WsAddressState](uri, msg => WsAddressState.format.reads(Json parse msg.asTextMessage.getStrictText).get, true) {

  def balances: Seq[Map[Asset, WsBalances]] = getMessagesBuffer.map(_.balances)
  def orders: Seq[Seq[WsOrder]]             = getMessagesBuffer.map(_.orders)

  def getBalancesSnapshot: Map[Asset, WsBalances] = balances.headOption.getOrElse(Map.empty)
  def getOrdersSnapshot: Seq[WsOrder]             = orders.headOption.getOrElse(Seq.empty)

  def getBalancesChanges: Seq[Map[Asset, WsBalances]] = balances.tail.distinct
  def getOrderChanges: Seq[WsOrder]                   = orders.tail.flatMap(_.toSeq).distinct
}
