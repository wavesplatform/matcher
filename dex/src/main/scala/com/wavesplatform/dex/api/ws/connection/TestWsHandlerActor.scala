package com.wavesplatform.dex.api.ws.connection

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.dex.api.ws.protocol.{WsClientMessage, WsPingOrPong}
import com.wavesplatform.dex.domain.utils.ScorexLogging

/**
  * Used as a proxy to the connection's source actor.
  * Main goal is to respond with pongs to matcher's pings to keep connection alive
  */
class TestWsHandlerActor(keepAlive: Boolean) extends Actor with ScorexLogging {

  import TestWsHandlerActor._

  private def awaitPings(sourceRef: ActorRef): Receive = {

    case p: WsPingOrPong => if (keepAlive) sourceRef ! p

    case CloseConnection =>
      log.debug("Closing connection")
      sourceRef ! akka.actor.Status.Success(None)
      context.stop(self)

    case SendToServer(message) =>
      log.debug(s"Manually sending: ${WsClientMessage.wsClientMessageWrites.writes(message)}")
      sourceRef ! message
  }

  override val receive: Receive = {
    case AssignSourceRef => context.become { awaitPings(sourceRef = sender) }
  }
}

object TestWsHandlerActor {

  def props(keepAlive: Boolean): Props = Props(new TestWsHandlerActor(keepAlive))

  final case object AssignSourceRef
  final case object CloseConnection
  final case class SendToServer(message: WsClientMessage)
}
