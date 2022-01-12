package com.wavesplatform.dex.api.ws.protocol

import akka.http.scaladsl.model.ws.TextMessage
import play.api.libs.json.{Json, Writes}

trait WsMessage {
  def tpe: String
}

object WsMessage {
  def toStrictTextMessage[T <: WsMessage: Writes](x: T): TextMessage.Strict = TextMessage.Strict(Json.toJson(x).toString)
}
