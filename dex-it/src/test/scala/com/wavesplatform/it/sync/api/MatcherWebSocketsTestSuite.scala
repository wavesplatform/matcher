package com.wavesplatform.it.sync.api

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import com.wavesplatform.it.MatcherSuiteBase

import scala.concurrent.{Future, Promise}

class MatcherWebSocketsTestSuite extends MatcherSuiteBase {

  "simple web socket test" in {

    implicit val system: ActorSystem        = ActorSystem()
    implicit val materializer: Materializer = Materializer.matFromSystem(system)

    val sink: Sink[Message, Future[Done]] = Sink.foreach {
      case message: TextMessage.Strict => message.text should startWith("Now is")
      case e                           => log.error(s"Impossible: $e")
    }

    // using Source.maybe materializes into a promise
    // which will allow us to complete the source later
    // see https://doc.akka.io/docs/akka-http/current/client-side/websocket-support.html#half-closed-websockets
    val flow: Flow[Message, Message, Promise[Option[Message]]] = Flow.fromSinkAndSourceMat(sink, Source.maybe[Message])(Keep.right)

    // upgradeResponse is a Future[WebSocketUpgradeResponse] that
    // completes or fails when the connection succeeds or fails
    val (_, closed) = Http().singleWebSocketRequest(WebSocketRequest(s"ws://127.0.0.1:${dex1.restApiAddress.getPort}/ws/time"), flow)

    // at some later time we want to disconnect
    Thread.sleep(3000)
    closed.success(None)
  }
}
