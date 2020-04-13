package com.wavesplatform.dex.it.api.websockets

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.Json

class WsAuthenticatedConnection(uri: String, apiKey: Option[String], keepAlive: Boolean = true)(implicit system: ActorSystem,
                                                                                                materializer: Materializer)
    extends WsConnection[WsAddressState](uri = uri,
                                         parseOutput = msg => Json.parse(msg.asTextMessage.getStrictText).as[WsAddressState],
                                         trackOutput = true,
                                         apiKey = apiKey,
                                         keepAlive = keepAlive) {

  def getBalancesChanges: Seq[Map[Asset, WsBalances]] = getMessagesBuffer.map(_.balances).filter(_.nonEmpty)
  def getOrderChanges: Seq[WsOrder]                   = getMessagesBuffer.flatMap(_.orders)
}
