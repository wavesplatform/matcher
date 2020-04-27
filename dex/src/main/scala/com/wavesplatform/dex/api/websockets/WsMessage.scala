package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage

trait WsMessage {
  def toStrictTextMessage: TextMessage.Strict
  def tpe: String
}

object WsMessage {

  // Will be never propagated to a client
  case object Complete extends WsMessage {
    override val toStrictTextMessage: TextMessage.Strict = TextMessage.Strict("{}")
    override val tpe: String                             = "unused"
  }
}
