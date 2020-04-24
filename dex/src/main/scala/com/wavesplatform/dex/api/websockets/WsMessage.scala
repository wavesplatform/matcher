package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage

trait WsMessage {
  def toStrictTextMessage: TextMessage.Strict
  def tpe: String
}

object WsMessage {

  private val unusedStrictTextMessage = TextMessage.Strict("{}")
  private val unusedTpe               = "unused"

  // Will be never propagated to a client
  case object Complete extends WsMessage {
    override val toStrictTextMessage: TextMessage.Strict = unusedStrictTextMessage
    override val tpe: String                             = unusedTpe
  }

  case class CompleteWithFailure(cause: Throwable) extends WsMessage {
    override val toStrictTextMessage: TextMessage.Strict = unusedStrictTextMessage
    override val tpe: String                             = unusedTpe
  }
}
