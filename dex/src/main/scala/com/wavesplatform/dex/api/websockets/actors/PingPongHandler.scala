package com.wavesplatform.dex.api.websockets.actors

import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.http.scaladsl.model.ws.TextMessage
import com.wavesplatform.dex.api.websockets.WsMessage
import com.wavesplatform.dex.api.websockets.actors.PingPongHandler._
import com.wavesplatform.dex.api.websockets.statuses.TerminationStatus
import com.wavesplatform.dex.api.websockets.statuses.TerminationStatus.{MaxLifetimeExceeded, PongTimeout}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration._

class PingPongHandler(settings: Settings) extends Actor {

  import context.dispatcher

  scheduleNextPingAll()

  private def scheduleNextPingAll(): Unit = context.system.scheduler.scheduleOnce(settings.pingInterval, self, PingAll)

  private def scheduleConnectionClosing(id: UUID): Cancellable = {
    context.system.scheduler.scheduleOnce(settings.pongTimeout, self, CloseConnection(TerminationStatus.PongTimeout(id)))
  }

  private def awaitPongs(requested: Map[UUID, (ActorRef, Cancellable)], responded: Map[UUID, ActorRef]): Receive = {

    case PingAll =>
      if (responded.nonEmpty) {
        val connectionsWithCloseCommands = responded.map { case (id, connection) => id -> (connection -> scheduleConnectionClosing(id)) } ++ requested
        context.become { awaitPongs(connectionsWithCloseCommands, Map.empty) }
        responded.foreach { case (id, connection) => connection ! Ping(id) }
      }
      scheduleNextPingAll()

    case AddConnection(id) => context.become { awaitPongs(requested, responded + (id -> sender)) }

    case CloseConnection(terminationStatus) =>
      requested.get(terminationStatus.connectionId).foreach {
        case (connection, _) =>
          context.become { awaitPongs(requested - terminationStatus.connectionId, responded) }
          connection ! (
            terminationStatus match {
              case _: PongTimeout         => Debug(s"PONG TIMEOUT reached (${settings.pongTimeout}), trying to close connection!")
              case _: MaxLifetimeExceeded => Debug(s"MAX LIFETIME EXCEEDED, trying to close connection!")
            }
          )
          connection ! akka.actor.Status.Success(terminationStatus)
      }

    case m @ TextMessage.Strict("d") =>
      requested.foreach {
        case (_, (connection, _)) =>
          connection ! Debug(
            s"Got debug message instead of pong! Current requested pings count = ${requested.size}, awaited ping count = ${responded.size}"
          )
      }

    case TextMessage.Strict(connectionId) =>
      val id = UUID.fromString(connectionId)
      requested.get(id).foreach {
        case (connection, cancelCommand) =>
          connection ! Debug(s"Got pong from $connectionId")
          cancelCommand.cancel()
          context.become { awaitPongs(requested - id, responded + (id -> connection)) }
      }
  }

  override def receive: Receive = awaitPongs(Map.empty, Map.empty)

}

object PingPongHandler {

  final case class Settings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)

  def props(settings: Settings): Props = Props(new PingPongHandler(settings))

  final case class AddConnection(id: UUID)
  final case class CloseConnection(terminationStatus: TerminationStatus)

  final case object PingAll

  // TODO Implement formal pong message!

  final case class Ping(connectionId: UUID) extends WsMessage {
    override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(Ping.format.writes(this).toString)
    override val tpe: String                             = "pp"
  }

  object Ping {
    implicit val format: Format[Ping] = Format(
      fjs = (__ \ "%").read[String].map(UUID.fromString _ andThen Ping.apply),
      tjs = ((__ \ "@").write[String] and (__ \ "%").write[String])(unlift(Ping.unapply) andThen (id => "pp" -> id.toString))
    )
  }

  // TODO Remove
  final case class Debug(message: String) extends WsMessage {
    override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(Json.format.writes(this).toString)
    override val tpe: String                             = "dg"
  }
}
