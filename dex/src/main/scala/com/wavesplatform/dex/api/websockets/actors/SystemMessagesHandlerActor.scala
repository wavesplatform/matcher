package com.wavesplatform.dex.api.websockets.actors

import java.util.UUID

import akka.actor.{Actor, Cancellable, Props}
import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import com.wavesplatform.dex.api.MatcherWebSocketRoute.ConnectionSource
import com.wavesplatform.dex.api.websockets.WsMessage
import com.wavesplatform.dex.api.websockets.actors.SystemMessagesHandlerActor._
import com.wavesplatform.dex.api.websockets.statuses.TerminationStatus
import com.wavesplatform.dex.api.websockets.statuses.TerminationStatus._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration._

class SystemMessagesHandlerActor(settings: Settings, maxConnectionLifetime: FiniteDuration, connectionSource: ConnectionSource)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  private val maxLifetimeExceeded = scheduleOnce(maxConnectionLifetime, CloseConnection(MaxLifetimeExceeded))
  private val firstPing           = scheduleOnce(settings.pingInterval, SendPing)

  private def scheduleOnce(delay: FiniteDuration, message: Any): Cancellable = context.system.scheduler.scheduleOnce(delay, self, message)
  private def schedulePongTimeout(): Cancellable                             = scheduleOnce(settings.pongTimeout, CloseConnection(PongTimeout))

  private def awaitPong(maybeExpectedPong: Option[PingOrPong], pongTimeout: Cancellable, nextPing: Cancellable): Receive = {

    case SendPing =>
      val (expectedPong, newNextPing)     = sendPingAndScheduleNextOne()
      val updatedPongTimeout: Cancellable = if (pongTimeout.isCancelled) schedulePongTimeout() else pongTimeout
      context.become { awaitPong(expectedPong.some, updatedPongTimeout, newNextPing) }

    case pong: PingOrPong =>
      maybeExpectedPong.fold { log.trace(s"Got unexpected pong: $pong") } { expectedPong =>
        if (pong == expectedPong) {
          pongTimeout.cancel()
          context.become { awaitPong(none, Cancellable.alreadyCancelled, nextPing) }
        } else log.trace(s"Got outdated pong: $pong")
      }

    case CloseConnection(terminationStatus) =>
      closeSourceAndCancelAllTasks(terminationStatus, List(nextPing, pongTimeout, maxLifetimeExceeded, firstPing))
  }

  private def closeSourceAndCancelAllTasks(terminationStatus: TerminationStatus, tasks: List[Cancellable]): Unit = {
    tasks.foreach { _.cancel() }
    val reasonText = terminationStatus match {
      case PongTimeout         => "WebSocket has reached pong timeout"
      case MaxLifetimeExceeded => "WebSocket has reached max allowed lifetime"
    }
    log.trace(s"[${connectionSource.name}] $reasonText")
    connectionSource.close(terminationStatus)
    context.stop(self)
  }

  private def sendPingAndScheduleNextOne(): (PingOrPong, Cancellable) = {
    val nextUuid = UUID.randomUUID()
    val ping     = PingOrPong(nextUuid)
    val nextPing = scheduleOnce(settings.pingInterval, SendPing)
    connectionSource.ping(ping)
    ping -> nextPing
  }

  override def receive: Receive = {
    case SendPing =>
      val (expectedPong, nextPing) = sendPingAndScheduleNextOne()
      val pongTimeout: Cancellable = schedulePongTimeout()
      context.become { awaitPong(expectedPong.some, pongTimeout, nextPing) }

    case CloseConnection(terminationStatus) => closeSourceAndCancelAllTasks(terminationStatus, List(maxLifetimeExceeded, firstPing))
  }
}

object SystemMessagesHandlerActor {

  final case class Settings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)

  def props(settings: Settings, maxConnectionLifetime: FiniteDuration, connectionSource: ConnectionSource): Props =
    Props(new SystemMessagesHandlerActor(settings, maxConnectionLifetime: FiniteDuration, connectionSource))

  final case object SendPing

  final case class CloseConnection(terminationStatus: TerminationStatus)

  final case class PingOrPong(connectionId: UUID, timestamp: Long = System.currentTimeMillis) extends WsMessage {
    override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(PingOrPong.format.writes(this).toString)
    override val tpe: String                             = "pp"
  }

  object PingOrPong {

    def wsUnapply(arg: PingOrPong): Option[(String, UUID, Long)] = (arg.tpe, arg.connectionId, arg.timestamp).some

    implicit val format: Format[PingOrPong] = (
      (__ \ "T").format[String] and
        (__ \ "_").format[Long] and
        (__ \ "%").format[String]
    )(
      (_, ts, uuid) => PingOrPong(UUID.fromString(uuid), ts),
      unlift(PingOrPong.wsUnapply) andThen { case (tpe, id, ts) => (tpe, ts, id.toString) }
    )
  }
}
