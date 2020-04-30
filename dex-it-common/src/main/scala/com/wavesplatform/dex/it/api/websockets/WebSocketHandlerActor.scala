package com.wavesplatform.dex.it.api.websockets

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.dex.api.websockets.{WsClientMessage, WsPingOrPong}
import com.wavesplatform.dex.domain.utils.ScorexLogging

/**
 * Used as a proxy to the connection's source actor.
 * Main goal is to respond with pongs to matcher's pings to keep connection alive
 */
class WebSocketHandlerActor(keepAlive: Boolean) extends Actor with ScorexLogging {

  import WebSocketHandlerActor._

  private def awaitPings(sourceRef: ActorRef): Receive = {

    case p: WsPingOrPong => if (keepAlive) sourceRef ! p

    case CloseConnection =>
      log.debug("Closing connection")
      sourceRef ! akka.actor.Status.Success(None)
      context.stop(self)

    case SendToServer(message) =>
      log.debug(s"Manually sending: ${message.toStrictTextMessage.getStrictText}")
      sourceRef ! message
  }

  override val receive: Receive = {
    case AssignSourceRef => context.become { awaitPings(sourceRef = sender) }
  }
}

object WebSocketHandlerActor {

  def props(keepAlive: Boolean): Props = Props(new WebSocketHandlerActor(keepAlive))

  final case object AssignSourceRef
  final case object CloseConnection
  final case class SendToServer(message: WsClientMessage)
}
