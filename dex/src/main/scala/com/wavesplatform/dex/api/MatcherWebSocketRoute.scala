package com.wavesplatform.dex.api

import java.util.UUID

import akka.Done
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorRef, typed}
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink}
import akka.stream.typed.scaladsl.{ActorSource, _}
import cats.syntax.either._
import akka.stream.{Materializer, OverflowStrategy}
import com.wavesplatform.dex.AssetPairBuilder
import com.wavesplatform.dex.api.http.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.api.websockets.actors.WsHandlerActor
import com.wavesplatform.dex.api.websockets.{WsClientMessage, WsMessage, WsServerMessage}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.settings.WebSocketSettings
import com.wavesplatform.dex.time.Time
import io.swagger.annotations.Api
import javax.ws.rs.Path
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.{Failure, Success}

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

  private def mkFailure(msg: String): Future[Nothing]                = Future.failed { new IllegalArgumentException(msg) }
  private val binaryMessageUnsupportedFailure: Future[Nothing]       = mkFailure("Binary messages are not supported")
  private def unexpectedMessageFailure(msg: String): Future[Nothing] = mkFailure(s"Got unexpected message instead of pong: $msg")

  override def route: Route = pathPrefix("ws") {
    commonWsRoute
  }

  private val completedSuccessfully: WsHandlerActor.Message = WsHandlerActor.Completed(().asRight)
  private val commonWsRoute: Route = (pathPrefix("v0") & pathEnd & get) {
    import webSocketSettings._

    val clientId = UUID.randomUUID().toString

    // From server to client
    val client = ActorSource
      .actorRef[WsServerMessage](
        { case WsServerMessage.Complete => },
        PartialFunction.empty,
        10,
        OverflowStrategy.fail
      )
      .named(s"source-$clientId")
      .map(WsMessage.toStrictTextMessage(_)(WsServerMessage.wsServerMessageWrites))
      .watchTermination()(handleTermination[WsServerMessage])

    val (clientRef, clientSource) = client.preMaterialize()
    val webSocketHandlerRef = mat.system.spawn(
      behavior = WsHandlerActor(webSocketHandler, time, assetPairBuilder, clientRef, matcher, addressDirectory),
      name = s"handler-$clientId"
    )

    val server = ActorSink
      .actorRef[WsHandlerActor.Message](
        ref = webSocketHandlerRef,
        onCompleteMessage = completedSuccessfully,
        onFailureMessage = e => WsHandlerActor.Completed(Left(e))
      )
      .named(s"server-$clientId")

    // From client to server
    val serverSink = Flow[Message]
      .mapAsync[WsHandlerActor.Command.ProcessClientMessage](1) {
        case tm: TextMessage =>
          for {
            strictText <- tm.toStrict(webSocketHandler.pingInterval / 5).map(_.getStrictText)
            clientMessage <- Json.parse(strictText).asOpt[WsClientMessage] match {
              case Some(x) => Future.successful(WsHandlerActor.Command.ProcessClientMessage(x))
              case None    => unexpectedMessageFailure(strictText)
            }
          } yield clientMessage

        case bm: BinaryMessage =>
          bm.dataStream.runWith(Sink.ignore)
          binaryMessageUnsupportedFailure
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
