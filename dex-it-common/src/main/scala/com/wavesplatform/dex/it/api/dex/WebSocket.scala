package com.wavesplatform.dex.it.api.dex

import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.it.docker.DexContainer

import scala.concurrent.Future

class WebSocket(account: KeyPair, dex: DexContainer) {
  implicit private val system: ActorSystem        = ActorSystem()
  implicit private val materializer: Materializer = Materializer.matFromSystem(system)

  private val sink: Sink[Message, Future[Done]] = Sink.foreach {
    case message: TextMessage.Strict => messages.put(messages.size, message.text)
    case e                           => println(s"Unexpected message error: $e")
  }

  private val response =
    Http().singleWebSocketRequest(WebSocketRequest(endpoint), Flow.fromSinkAndSourceMat(sink, Source.maybe[Message])(Keep.right))._2

  var messages = new ConcurrentHashMap[Int, String]() // ._2 could be replaced by something MatcherWsResponse

  def success(message: Option[Message] = None): Unit = response.success(message)

  def endpoint(): String = {
    s"ws://127.0.0.1:${dex.restApiAddress.getPort}/ws/time"  //TODO: replace with correct endpoint
  }
}
