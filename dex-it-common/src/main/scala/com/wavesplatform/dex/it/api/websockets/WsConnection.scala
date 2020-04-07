package com.wavesplatform.dex.it.api.websockets

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue

import akka.Done
import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest, WebSocketUpgradeResponse}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, Materializer, OverflowStrategy}
import cats.syntax.option._
import com.wavesplatform.dex.api.http.`X-Api-Key`
import com.wavesplatform.dex.api.websockets.WsMessage
import com.wavesplatform.dex.api.websockets.actors.PingPongHandlerActor.PingOrPong
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.websockets.WsConnection.PongHandler
import play.api.libs.json.Json

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class WsConnection[Output <: WsMessage: ClassTag](uri: String,
                                                  parseOutput: Message => Output,
                                                  trackOutput: Boolean,
                                                  apiKey: Option[String] = None,
                                                  keepAlive: Boolean = true)(implicit system: ActorSystem, materializer: Materializer)
    extends ScorexLogging {

  log.info(s"""Connecting to Matcher WS API:
            |         URI = ws://$uri
            |     API Key = $apiKey
            |  Keep alive = $keepAlive""".stripMargin)

  private val pongHandler = system.actorOf(PongHandler props keepAlive)

  private val source: Source[TextMessage.Strict, ActorRef] = {

    val completionMatcher: PartialFunction[Any, CompletionStrategy] = { case akka.actor.Status.Success(_) => CompletionStrategy.draining }
    val failureMatcher: PartialFunction[Any, Throwable]             = { case Status.Failure(cause)        => cause }

    Source
      .actorRef[PingOrPong](completionMatcher, failureMatcher, 10, OverflowStrategy.fail)
      .map(_.toStrictTextMessage)
      .mapMaterializedValue { source =>
        pongHandler.tell(PongHandler.AssignSourceRef, source)
        source
      }
  }

  private val messagesBuffer: ConcurrentLinkedQueue[Output]  = new ConcurrentLinkedQueue[Output]()
  private val pingsBuffer: ConcurrentLinkedQueue[PingOrPong] = new ConcurrentLinkedQueue[PingOrPong]()

  private val sink: Sink[Message, Future[Done]] = Sink.foreach { x =>
    val rawMsg = x.asTextMessage.getStrictText
    Try { parseOutput(x) } orElse Try { Json.parse(rawMsg).as[PingOrPong] } match {
      case Success(p: PingOrPong)  => log.debug(s"Got ping: $rawMsg${if (keepAlive) ", responding" else ""}"); pingsBuffer.add(p); pongHandler ! p
      case Success(output: Output) => if (trackOutput) messagesBuffer.add(output); log.info(s"Got message: $rawMsg")
      case Failure(e)              => log.error(s"Can't parse message: ${x.asTextMessage.getStrictText}", e)
    }
  }

  // maybe this flow can be made in more natural for Akka Streams way, especially pong handling by source
  private val flow: Flow[Message, TextMessage.Strict, Future[Done]] = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() {
    case (_, f) =>
      f.onComplete {
        case Success(_) => log.info(s"WebSocket connection to ws://$uri successfully closed")
        case Failure(e) => log.error(s"WebSocket connection to ws://$uri closed with an error", e)
      }(materializer.executionContext)
      f
  }

  private val (response, closed) = {
    val apiKeyHeaders = apiKey.fold(List.empty[HttpHeader])(apiKeyStr => List(`X-Api-Key`(apiKeyStr)))
    Http().singleWebSocketRequest(WebSocketRequest(s"ws://$uri", apiKeyHeaders), flow)
  }

  def getUri: String                                          = uri
  def getConnectionResponse: Future[WebSocketUpgradeResponse] = response

  def getMessagesBuffer: Seq[Output] = messagesBuffer.iterator().asScala.toSeq
  def clearMessagesBuffer(): Unit    = messagesBuffer.clear()

  def getPingsBuffer: Seq[PingOrPong] = pingsBuffer.iterator().asScala.toSeq
  def clearPingsBuffer(): Unit        = pingsBuffer.clear()

  def sendPong(pong: PingOrPong): Unit = pongHandler ! PongHandler.ManualPong(pong)

  def close(): Unit     = if (!isClosed) pongHandler ! PongHandler.CloseConnection
  def isClosed: Boolean = closed.isCompleted
}

object WsConnection {

  /**
    * Used as a proxy to the connection's source actor.
    * Main goal is to respond with pongs to matcher's pings to keep connection alive
    */
  class PongHandler(keepAlive: Boolean) extends Actor with ScorexLogging {

    import PongHandler._

    private def awaitPings(sourceRef: ActorRef, connectionId: Option[UUID]): Receive = {

      case p @ PingOrPong(id, _) => if (connectionId.isEmpty) context.become { awaitPings(sourceRef, id.some) }; if (keepAlive) sourceRef ! p

      case CloseConnection =>
        connectionId
          .fold { log.debug(s"Closing connection (id wasn't assigned by matcher yet)") } { id =>
            log.debug(s"Closing connection $id")
          }
        sourceRef ! akka.actor.Status.Success(None)
        context.become(awaitSourceRef)

      case ManualPong(pong) => log.debug(s"Manually sending pong: ${pong.toStrictTextMessage.getStrictText}"); sourceRef ! pong
    }

    private def awaitSourceRef: Receive = {
      case AssignSourceRef => context.become { awaitPings(sourceRef = sender, connectionId = None) }
    }

    override def receive: Receive = awaitSourceRef
  }

  object PongHandler {

    def props(keepAlive: Boolean): Props = Props(new PongHandler(keepAlive))

    final case object AssignSourceRef
    final case object CloseConnection
    final case class ManualPong(pong: PingOrPong)
  }
}
