package com.wavesplatform.dex.api.websockets.actors

import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.http.scaladsl.model.ws.TextMessage
import com.wavesplatform.dex.api.websockets.WsMessage
import com.wavesplatform.dex.api.websockets.actors.PingPongHandler._
import com.wavesplatform.dex.api.websockets.statuses.TerminationStatus
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration._

class PingPongHandler(settings: Settings) extends Actor with ScorexLogging {

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
        responded.foreach { case (id, connection) => connection ! PingOrPong(id) }
      }
      scheduleNextPingAll()

    case AddConnection(id) => context.become { awaitPongs(requested, responded + (id -> sender)) }

    case CloseConnection(terminationStatus) =>
      val closingConnId = terminationStatus.connectionId

      lazy val closeRequested = requested.get(closingConnId).map { case (conn, _) => conn -> awaitPongs(requested - closingConnId, responded) }
      lazy val closeResponded = responded.get(closingConnId).map { _ -> awaitPongs(requested, responded - closingConnId) }

      (closeRequested orElse closeResponded).foreach {
        case (connection, newBehaviour) =>
          context.become(newBehaviour)
          connection ! akka.actor.Status.Success(terminationStatus)
      }

    case TextMessage.Strict(maybePong) =>
      Json
        .parse(maybePong)
        .asOpt[PingOrPong]
        .fold { log.error(s"Got unexpected message instead of pong: $maybePong") } { pong =>
          requested.get(pong.connectionId).foreach {
            case (connection, cancelCommand) =>
              cancelCommand.cancel()
              context.become { awaitPongs(requested - pong.connectionId, responded + (pong.connectionId -> connection)) }
          }
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

  final case class PingOrPong(connectionId: UUID) extends WsMessage {
    override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(PingOrPong.format.writes(this).toString)
    override val tpe: String                             = "pp"
  }

  object PingOrPong {
    implicit val format: Format[PingOrPong] = Format(
      fjs = (__ \ "%").read[String].map(UUID.fromString _ andThen PingOrPong.apply),
      tjs = ((__ \ "@").write[String] and (__ \ "%").write[String])(unlift(PingOrPong.unapply) andThen (id => "pp" -> id.toString))
    )
  }
}
