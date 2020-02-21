package com.wavesplatform.dex.it.api.websockets

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, WebSocketRequest}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

case class WebSocketConnection[Output](uri: String, parseOutput: Message => Output, trackOutput: Boolean)(implicit system: ActorSystem,
                                                                                                          materializer: Materializer) {

  private val messagesBuffer: mutable.Queue[Output] = mutable.Queue.empty

  private val sink: Sink[Message, Future[Done]] = Sink.foreach { parseOutput andThen (output => if (trackOutput) messagesBuffer enqueue output) }

  // using Source.maybe materializes into a promise
  // which will allow us to complete the source later
  // see https://doc.akka.io/docs/akka-http/current/client-side/websocket-support.html#half-closed-websockets
  private val flow: Flow[Message, Message, Promise[Option[Message]]] = Flow.fromSinkAndSourceMat(sink, Source.maybe[Message])(Keep.right)
  private val (_, closed)                                            = Http().singleWebSocketRequest(WebSocketRequest(s"ws://127.0.0.1:$uri"), flow)

  def getMessagesBuffer: mutable.Queue[Output] = messagesBuffer

  def clearMessagesBuffer(): Unit = messagesBuffer.clear()

  def close(): Unit = closed.success(None)

  def isClosed: Boolean = closed.isCompleted
}
