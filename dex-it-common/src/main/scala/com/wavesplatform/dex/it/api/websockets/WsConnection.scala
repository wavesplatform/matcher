package com.wavesplatform.dex.it.api.websockets

import java.util.concurrent.ConcurrentLinkedQueue

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.ws.{Message, WebSocketRequest, WebSocketUpgradeResponse}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import com.wavesplatform.dex.api.http.`X-Api-Key`
import com.wavesplatform.dex.domain.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class WsConnection[Output](uri: String, parseOutput: Message => Output, trackOutput: Boolean, apiKey: Option[String] = None)(
    implicit system: ActorSystem,
    materializer: Materializer)
    extends ScorexLogging {

  log.info(s"Connecting to ws://$uri, apiKey = $apiKey")

  private val messagesBuffer: ConcurrentLinkedQueue[Output] = new ConcurrentLinkedQueue[Output]()

  private val sink: Sink[Message, Future[Done]] = Sink.foreach { x =>
    try {
      val output = parseOutput(x)
      if (trackOutput) messagesBuffer.add(output)
      log.info(s"Got message: ${x.asTextMessage.getStrictText}")
    } catch {
      case e: Throwable => log.error(s"Can't parse message: $x", e)
    }
  }

  // using Source.maybe materializes into a promise
  // which will allow us to complete the source later
  // see https://doc.akka.io/docs/akka-http/current/client-side/websocket-support.html#half-closed-websockets
  private val flow: Flow[Message, Message, Promise[Option[Message]]] =
    Flow.fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.right).watchTermination() { (r, f) =>
      f.onComplete {
        case Success(_) => log.trace("WebSocket connection successfully closed")
        case Failure(e) => log.trace(s"WebSocket connection closed with an error: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
      }(materializer.executionContext)
      r
    }

  private val (response, closed) = {
    val apiKeyHeaders = apiKey.fold(List.empty[HttpHeader])(apiKeyStr => List(`X-Api-Key`(apiKeyStr)))
    Http().singleWebSocketRequest(WebSocketRequest(s"ws://$uri", apiKeyHeaders), flow)
  }

  def getConnectionResponse: Future[WebSocketUpgradeResponse] = response

  def getMessagesBuffer: Seq[Output] = messagesBuffer.iterator().asScala.toSeq

  def clearMessagesBuffer(): Unit = messagesBuffer.clear()

  def close(): Unit = if (!isClosed) closed.success(None)

  def isClosed: Boolean = closed.isCompleted
}
