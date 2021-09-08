package com.wavesplatform.dex.load.ws

import java.util.concurrent.ThreadLocalRandom

import akka.Done
import akka.actor.{ActorRef, ActorSystem, Status}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, Materializer, OverflowStrategy}
import com.wavesplatform.dex.api.ws.connection.TestWsHandlerActor
import com.wavesplatform.dex.api.ws.protocol.{WsClientMessage, WsMessage, WsServerMessage}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class WsConnection(uri: String, receive: WsServerMessage => Option[WsClientMessage])(implicit system: ActorSystem) extends ScorexLogging {

  import system.dispatcher
  private val testId: Int = ThreadLocalRandom.current().nextInt(10000)
  implicit private val materializer = Materializer(system)
  private val wsHandlerRef = system.actorOf(TestWsHandlerActor.props(testId, keepAlive = true))

  log.info(s"Connecting to Matcher WS API: $uri")

  protected def stringifyClientMessage(cm: WsClientMessage): TextMessage.Strict =
    WsMessage.toStrictTextMessage(cm)(WsClientMessage.wsClientMessageWrites)

  // To server
  private val source: Source[TextMessage.Strict, ActorRef] = {
    val completionMatcher: PartialFunction[Any, CompletionStrategy] = { case akka.actor.Status.Success(_) => CompletionStrategy.draining }
    val failureMatcher: PartialFunction[Any, Throwable] = { case Status.Failure(cause) => cause }

    Source
      .actorRef[WsClientMessage](completionMatcher, failureMatcher, 10, OverflowStrategy.fail)
      .map(stringifyClientMessage)
      .mapMaterializedValue { source =>
        wsHandlerRef.tell(TestWsHandlerActor.AssignSourceRef, source)
        source
      }
  }

  // To client
  private val sink: Sink[Message, Future[Done]] = Sink.foreachAsync(1) {
    case tm: TextMessage => // TODO move to tests
      for {
        strictText <- tm.toStrict(1.second).map(_.getStrictText)
        _ <- {
          log.trace(s"Got $strictText")
          Try(Json.parse(strictText).as[WsServerMessage]) match {
            case Failure(exception) => Future.failed(exception)
            case Success(x) => Future.successful(receive(x).foreach(wsHandlerRef ! _))
          }
        }
      } yield Done

    case bm: BinaryMessage =>
      bm.dataStream.runWith(Sink.ignore)
      Future.failed(new IllegalArgumentException("Binary messages are not supported"))
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

  def isClosed: Boolean = closed.isCompleted

  def close(): Future[Done] = {
    if (!isClosed) wsHandlerRef ! TestWsHandlerActor.CloseConnection
    closed
  }

}
