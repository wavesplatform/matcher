package com.wavesplatform.dex.load.ws

import akka.Done
import akka.actor.{ActorRef, ActorSystem, Status}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, Materializer, OverflowStrategy}
import com.wavesplatform.dex.api.websockets.connection.TestWsHandlerActor
import com.wavesplatform.dex.api.websockets.{WsClientMessage, WsMessage, WsServerMessage}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.json.{JsError, JsSuccess, Json}

import scala.concurrent.Future
import scala.util.{Failure, Success}

class WsConnection(uri: String, receive: WsServerMessage => Option[WsClientMessage])(implicit system: ActorSystem) extends ScorexLogging {

  private implicit val materializer = Materializer(system)
  private val wsHandlerRef          = system.actorOf(TestWsHandlerActor.props(keepAlive = true))

  log.info(s"Connecting to Matcher WS API: $uri")

  protected def stringifyClientMessage(cm: WsClientMessage): TextMessage.Strict =
    WsMessage.toStrictTextMessage(cm)(WsClientMessage.wsClientMessageWrites)

  // To server
  private val source: Source[TextMessage.Strict, ActorRef] = {
    val completionMatcher: PartialFunction[Any, CompletionStrategy] = { case akka.actor.Status.Success(_) => CompletionStrategy.draining }
    val failureMatcher: PartialFunction[Any, Throwable]             = { case Status.Failure(cause)        => cause }

    Source
      .actorRef[WsClientMessage](completionMatcher, failureMatcher, 10, OverflowStrategy.fail)
      .map(stringifyClientMessage)
      .mapMaterializedValue { source =>
        wsHandlerRef.tell(TestWsHandlerActor.AssignSourceRef, source)
        source
      }
  }

  // To client
  private val sink: Sink[Message, Future[Done]] = Sink.foreach { x =>
    val rawMsg = x.asTextMessage.getStrictText
    Json.parse(rawMsg).validate[WsServerMessage] match {
      case JsError(e) => log.error(s"Can't parse message: $rawMsg, $e")
      case JsSuccess(value, _) =>
        log.trace(s"Got $value")
        receive(value).foreach(wsHandlerRef ! _)
    }
  }

  private val flow: Flow[Message, TextMessage.Strict, Future[Done]] = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() {
    case (_, f) =>
      f.onComplete {
        case Success(_) => log.info(s"WebSocket connection to $uri successfully closed")
        case Failure(e) => log.error(s"WebSocket connection to $uri closed with an error", e)
      }(materializer.executionContext)
      f
  }

  val (connectionResponse, closed) = Http().singleWebSocketRequest(WebSocketRequest(uri), flow)

  def send(message: WsClientMessage): Unit = wsHandlerRef ! TestWsHandlerActor.SendToServer(message)
  def close(): Unit                        = if (!isClosed) wsHandlerRef ! TestWsHandlerActor.CloseConnection
  def isClosed: Boolean                    = closed.isCompleted
}
