package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage

trait WsMessage {
  def toStrictTextMessage: TextMessage.Strict
  def tpe: String
}

object WsMessage {
  case object Complete extends WsMessage {
    override def toStrictTextMessage: TextMessage.Strict = throw new NotImplementedError("toStrictTextMessage")
    override def tpe: String = throw new NotImplementedError("tpe")
  }
}
