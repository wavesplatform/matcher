package com.wavesplatform.dex.api

import java.util.UUID

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorRef, typed}
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.typed.scaladsl.{ActorSource, _}
import akka.stream.{Materializer, OverflowStrategy}
import akka.{Done, NotUsed}
import cats.syntax.either._
import com.wavesplatform.dex.AssetPairBuilder
import com.wavesplatform.dex.api.http.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.api.ws.actors.WsHandlerActor
import com.wavesplatform.dex.api.ws.{WsClientMessage, WsMessage, WsServerMessage}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.InvalidJson
import com.wavesplatform.dex.settings.WebSocketSettings
import com.wavesplatform.dex.time.Time
import io.swagger.annotations.Api
import javax.ws.rs.Path
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

@Path("/ws")
@Api(value = "/web sockets/")
case class MatcherWebSocketRoute(addressDirectory: ActorRef,
                                 matcher: ActorRef,
                                 time: Time,
                                 assetPairBuilder: AssetPairBuilder,
                                 orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                                 apiKeyHash: Option[Array[Byte]],
                                 webSocketSettings: WebSocketSettings)(implicit mat: Materializer)
    extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  import mat.executionContext

  override def route: Route = pathPrefix("ws") { commonWsRoute }

  private val commonWsRoute: Route = (pathPrefix("v0") & pathEnd & get) {
    import webSocketSettings._

    val clientId = UUID.randomUUID().toString

    // From server to client
    val client: Source[TextMessage.Strict, typed.ActorRef[WsServerMessage]] =
      ActorSource
        .actorRef[WsServerMessage](
          { case WsServerMessage.Complete => },
          PartialFunction.empty,
          100,
          OverflowStrategy.fail
        )
        .named(s"source-$clientId")
        .map(WsMessage.toStrictTextMessage(_)(WsServerMessage.wsServerMessageWrites))
        .watchTermination()(handleTermination[WsServerMessage])

    val (clientRef, clientSource) = client.preMaterialize()

    val webSocketHandlerRef: typed.ActorRef[WsHandlerActor.Message] =
      mat.system.spawn(
        behavior = WsHandlerActor(webSocketHandler, time, assetPairBuilder, clientRef, matcher, addressDirectory, clientId),
        name = s"handler-$clientId"
      )

    val server: Sink[WsHandlerActor.Message, NotUsed] =
      ActorSink
        .actorRef[WsHandlerActor.Message](
          ref = webSocketHandlerRef,
          onCompleteMessage = WsHandlerActor.Event.Completed(().asRight),
          onFailureMessage = e => WsHandlerActor.Event.Completed(e.asLeft)
        )
        .named(s"server-$clientId")

    // From client to server
    val serverSink: Sink[Message, NotUsed] =
      Flow[Message]
        .mapAsync[WsHandlerActor.Command](1) {
          case tm: TextMessage =>
            val parseResult =
              for {
                strictText <- tm.toStrict(webSocketHandler.pingInterval / 5).map(_.getStrictText)
                clientMessage <- Try { Json.parse(strictText).as[WsClientMessage] } match {
                  case Success(cm)        => Future.successful { WsHandlerActor.Command.ProcessClientMessage(cm) }
                  case Failure(exception) => Future.failed(exception)
                }
              } yield clientMessage

            parseResult.recover { case _ => WsHandlerActor.Command.ForwardClientError(InvalidJson(Nil)) }

          case bm: BinaryMessage =>
            bm.dataStream.runWith(Sink.ignore)
            Future.failed { new IllegalArgumentException("Binary messages are not supported") }
        }
        .named(s"sink-$clientId")
        .to(server)

    handleWebSocketMessages { Flow.fromSinkAndSourceCoupled(serverSink, clientSource) }
  }

  private def handleTermination[T](client: typed.ActorRef[T], r: Future[Done]): typed.ActorRef[T] = {
    val cn = client.path.name
    r.onComplete {
      case Success(_) => log.trace(s"[c=$cn] WebSocket connection successfully closed")
      case Failure(e) => log.trace(s"[c=$cn] WebSocket connection closed with an error: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
    }(mat.executionContext)
    client
  }
}
