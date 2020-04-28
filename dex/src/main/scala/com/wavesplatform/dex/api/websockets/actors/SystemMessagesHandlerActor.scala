package com.wavesplatform.dex.api.websockets.actors

import akka.actor.{Actor, Cancellable, Props, typed}
import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.WsMessage
import com.wavesplatform.dex.api.websockets.actors.SystemMessagesHandlerActor._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.{MatcherError, WsConnectionMaxLifetimeExceeded, WsConnectionPongTimeout}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration._

class SystemMessagesHandlerActor(settings: Settings, maxConnectionLifetime: FiniteDuration, sourceActor: typed.ActorRef[WsMessage])
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  private val maxLifetimeExceeded = scheduleOnce(maxConnectionLifetime, CloseConnection(WsConnectionMaxLifetimeExceeded))
  private val firstPing           = scheduleOnce(settings.pingInterval, SendPing)

  private def scheduleOnce(delay: FiniteDuration, message: Any): Cancellable = context.system.scheduler.scheduleOnce(delay, self, message)
  private def schedulePongTimeout(): Cancellable                             = scheduleOnce(settings.pongTimeout, CloseConnection(WsConnectionPongTimeout))

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

    case CloseConnection(reason) => closeSourceAndCancelAllTasks(reason, List(nextPing, pongTimeout, maxLifetimeExceeded, firstPing))
  }

  private def closeSourceAndCancelAllTasks(reason: MatcherError, tasks: List[Cancellable]): Unit = {
    tasks.foreach { _.cancel() }
    log.trace(s"[${sourceActor.path.name}] ${reason.message.text}")
    sourceActor ! WsMessage.Complete
    context.stop(self)
  }

  private def sendPingAndScheduleNextOne(): (PingOrPong, Cancellable) = {
    val ping     = PingOrPong(System.currentTimeMillis)
    val nextPing = scheduleOnce(settings.pingInterval, SendPing)
    sourceActor ! ping
    ping -> nextPing
  }

  override def receive: Receive = {
    case SendPing =>
      val (expectedPong, nextPing) = sendPingAndScheduleNextOne()
      val pongTimeout: Cancellable = schedulePongTimeout()
      context.become { awaitPong(expectedPong.some, pongTimeout, nextPing) }

    case CloseConnection(reason) => closeSourceAndCancelAllTasks(reason, List(maxLifetimeExceeded, firstPing))
  }
}

object SystemMessagesHandlerActor {

  final case class Settings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)

  def props(settings: Settings, maxConnectionLifetime: FiniteDuration, sourceActor: typed.ActorRef[WsMessage]): Props =
    Props(new SystemMessagesHandlerActor(settings, maxConnectionLifetime: FiniteDuration, sourceActor))

  final case object SendPing

  final case class CloseConnection(reason: MatcherError)

  final case class PingOrPong(timestamp: Long) extends WsMessage {
    override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(PingOrPong.format.writes(this).toString)
    override val tpe: String                             = "pp"
  }

  object PingOrPong {

    def wsUnapply(arg: PingOrPong): Option[(String, Long)] = (arg.tpe, arg.timestamp).some

    implicit val format: Format[PingOrPong] = (
      (__ \ "T").format[String] and
        (__ \ "_").format[Long]
    )(
      (_, ts) => PingOrPong(ts),
      unlift(PingOrPong.wsUnapply) andThen { case (tpe, ts) => (tpe, ts) }
    )
  }
}
