package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage

trait WsMessage {
  def toStrictTextMessage: TextMessage.Strict
  val tpe: String
}
