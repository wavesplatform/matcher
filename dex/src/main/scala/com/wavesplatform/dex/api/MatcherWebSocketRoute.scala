package com.wavesplatform.dex.api

import java.util.UUID

import akka.Done
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorRef, typed}
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink}
import akka.stream.typed.scaladsl.{ActorSource, _}
import akka.stream.{Materializer, OverflowStrategy}
import com.wavesplatform.dex.AssetPairBuilder
import com.wavesplatform.dex.api.http.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.api.websockets.actors.WebSocketHandlerActor
import com.wavesplatform.dex.api.websockets.{WsClientMessage, WsMessage}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.settings.WebSocketSettings
import io.swagger.annotations.Api
import javax.ws.rs.Path
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.{Failure, Success}

@Path("/ws")
@Api(value = "/web sockets/")
case class MatcherWebSocketRoute(addressDirectory: ActorRef,
                                 matcher: ActorRef,
                                 assetPairBuilder: AssetPairBuilder,
                                 orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                                 apiKeyHash: Option[Array[Byte]],
                                 webSocketSettings: WebSocketSettings)(implicit mat: Materializer)
    extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  import mat.executionContext

  private implicit val trm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller // TODO remove ???

  private def mkFailure(msg: String): Future[Nothing]                = Future.failed { new IllegalArgumentException(msg) }
  private val binaryMessageUnsupportedFailure: Future[Nothing]       = mkFailure("Binary messages are not supported")
  private def unexpectedMessageFailure(msg: String): Future[Nothing] = mkFailure(s"Got unexpected message instead of pong: $msg")

  override def route: Route = pathPrefix("ws") {
    commonWsRoute
  }

  private val commonWsRoute: Route = (pathEnd & get) {
    import webSocketSettings._

    val clientId = UUID.randomUUID().toString
    val client = ActorSource
      .actorRef[WsMessage](
        { case WsMessage.Complete => },
        PartialFunction.empty,
        10,
        OverflowStrategy.fail
      )
      .named(s"source-$clientId")
      .map(_.toStrictTextMessage)
      .watchTermination()(handleTermination)

    val (clientRef, clientSource) = client.preMaterialize()
    val webSocketHandlerRef = mat.system.spawn(
      behavior = WebSocketHandlerActor(webSocketHandler, maxConnectionLifetime, clientRef, matcher, addressDirectory),
      name = s"handler-$clientId"
    )

    val server = ActorSink
      .actorRef(
        ref = webSocketHandlerRef,
        onCompleteMessage = WebSocketHandlerActor.Command.Stop,
        onFailureMessage = WebSocketHandlerActor.Command.ProcessClientError
      )
      .named(s"server-$clientId")

    val serverSink = Flow[Message]
      .mapAsync[WebSocketHandlerActor.Command.ProcessClientMessage](1) {
        case tm: TextMessage =>
          for {
            strictText <- tm.toStrict(webSocketHandler.pingInterval / 5).map(_.getStrictText)
            pong <- Json.parse(strictText).asOpt(WsClientMessage.wsClientMessageReads) match {
              case Some(x) => Future.successful(WebSocketHandlerActor.Command.ProcessClientMessage(x))
              case None    => unexpectedMessageFailure(strictText)
            }
          } yield pong

        case bm: BinaryMessage =>
          bm.dataStream.runWith(Sink.ignore)
          binaryMessageUnsupportedFailure
      }
      .named(s"sink-$clientId")
      .to(server)

    handleWebSocketMessages { Flow.fromSinkAndSourceCoupled(serverSink, clientSource) }
  }

  private def handleTermination(client: typed.ActorRef[WsMessage], r: Future[Done]): typed.ActorRef[WsMessage] = {
    val cn = client.path.name
    r.onComplete {
      case Success(_) => log.trace(s"[c=$cn] WebSocket connection successfully closed")
      case Failure(e) => log.trace(s"[c=$cn] WebSocket connection closed with an error: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
    }(mat.executionContext)
    client
  }
}
