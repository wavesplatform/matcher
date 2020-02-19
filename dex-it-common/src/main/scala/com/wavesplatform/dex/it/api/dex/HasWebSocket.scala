package com.wavesplatform.dex.it.api.dex

import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.{Materializer}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.it.docker.DexContainer

import scala.concurrent.{Future, Promise}

trait HasWebSocket {
  implicit protected val system: ActorSystem        = ActorSystem()
  implicit protected val materializer: Materializer = Materializer.matFromSystem(system)

  val sink: Sink[Message, Future[Done]] = Sink.foreach {
    case message: TextMessage.Strict => wsMessages.put(wsMessages.size, message.text) //TODO: replace 'wsMessages.size' to something else
    case e                           => println(s"Unexpected message error: $e")
  }

  var wsMessages = new ConcurrentHashMap[Int, String]() //TODO: ._2 could be replaced by something like MatcherWsResponse

  def mkWebSocket(account: KeyPair, dex: DexContainer): Promise[Option[Message]] = {
    val endpoint = s"ws://127.0.0.1:${dex.restApiAddress.getPort}/ws/time"

    Http().singleWebSocketRequest(WebSocketRequest(endpoint), Flow.fromSinkAndSourceMat(sink, Source.maybe[Message])(Keep.right))._2
  }

  def terminateWs(): Unit = {
    wsMessages.clear
    materializer.shutdown
    system.terminate
  }
}
