package com.wavesplatform.dex.api

import java.time.LocalDateTime

import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.wavesplatform.dex.api.http.ApiRoute
import io.swagger.annotations.Api
import javax.ws.rs.Path

import scala.concurrent.duration._

@Path("/ws")
@Api(value = "/web sockets/")
case class MatcherWebSocketRoute()(implicit mat: Materializer) extends ApiRoute {

  private def greeter: Flow[Message, Message, Any] = Flow[Message].mapConcat {
    case tm: TextMessage   => TextMessage(Source.single("Hello ") ++ tm.textStream ++ Source.single("!")) :: Nil
    case bm: BinaryMessage => bm.dataStream.runWith(Sink.ignore); Nil
  }

  private def time: Flow[Message, Message, _] = {

    val sink   = Sink.cancelled[Message]
    val source = Source.tick(1.second, 1.second, "").map(_ => TextMessage(s"Now is ${LocalDateTime.now}"))

    Flow.fromSinkAndSource(sink, source)
  }

  override def route: Route = pathPrefix("ws") {
    path("greeter") {
      get {
        handleWebSocketMessages(greeter)
      }
    } ~ path("time") {
      get {
        handleWebSocketMessages(time)
      }
    }
  }
}
