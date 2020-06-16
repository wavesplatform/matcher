package com.wavesplatform.dex.api

import java.util.UUID

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorRef, typed}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.typed.scaladsl.{ActorSource, _}
import akka.stream.{Materializer, OverflowStrategy}
import akka.{Done, NotUsed}
import cats.syntax.either._
import com.wavesplatform.dex.api.http.{ApiRoute, AuthRoute}
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.api.websockets.actors.{WsExternalClientHandlerActor, WsInternalBroadcastActor, WsInternalClientHandlerActor}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.InvalidJson
import com.wavesplatform.dex.settings.WebSocketSettings
import com.wavesplatform.dex.time.Time
import com.wavesplatform.dex.{AssetPairBuilder, Matcher, error}
import play.api.libs.json.{Json, Reads}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

case class MatcherWebSocketRoute(wsInternalBroadcastRef: typed.ActorRef[WsInternalBroadcastActor.Command],
                                 addressDirectory: ActorRef,
                                 matcher: ActorRef,
                                 time: Time,
                                 assetPairBuilder: AssetPairBuilder,
                                 orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                                 apiKeyHash: Option[Array[Byte]],
                                 webSocketSettings: WebSocketSettings,
                                 matcherStatus: () => Matcher.Status)(implicit mat: Materializer)
    extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  import mat.executionContext

  override def route: Route = pathPrefix("ws" / "v0") {
    matcherStatusBarrier {
      internalWsRoute ~ commonWsRoute
    }
  }

  private val commonWsRoute: Route = (pathEnd & get) {
    import webSocketSettings.externalClientHandler

    val clientId = UUID.randomUUID().toString

    val client                    = mkSource(clientId)
    val (clientRef, clientSource) = client.preMaterialize()

    val webSocketHandlerRef: typed.ActorRef[WsExternalClientHandlerActor.Message] =
      mat.system.spawn(
        behavior = WsExternalClientHandlerActor(externalClientHandler, time, assetPairBuilder, clientRef, matcher, addressDirectory, clientId),
        name = s"handler-$clientId"
      )

    val server: Sink[WsExternalClientHandlerActor.Message, NotUsed] =
      ActorSink
        .actorRef[WsExternalClientHandlerActor.Message](
          ref = webSocketHandlerRef,
          onCompleteMessage = WsExternalClientHandlerActor.Event.Completed(().asRight),
          onFailureMessage = e => WsExternalClientHandlerActor.Event.Completed(e.asLeft)
        )
        .named(s"server-$clientId")

    val serverSink: Sink[Message, NotUsed] =
      mkServerSink[WsClientMessage, WsExternalClientHandlerActor.Command](clientId, externalClientHandler.healthCheck.pingInterval / 5) {
        case Right(x) => WsExternalClientHandlerActor.Command.ProcessClientMessage(x)
        case Left(x)  => WsExternalClientHandlerActor.Command.ForwardToClient(x)
      }.to(server)

    handleWebSocketMessages { Flow.fromSinkAndSourceCoupled(serverSink, clientSource) }
  }

  private val internalWsRoute: Route = (path("internal") & get) {
    import webSocketSettings.internalClientHandler

    val clientId = UUID.randomUUID().toString

    val client                    = mkSource(clientId)
    val (clientRef, clientSource) = client.preMaterialize()

    val webSocketHandlerRef: typed.ActorRef[WsInternalClientHandlerActor.Message] =
      mat.system.spawn(
        behavior = WsInternalClientHandlerActor(internalClientHandler, time, assetPairBuilder, clientRef, matcher, addressDirectory, clientId),
        name = s"handler-$clientId"
      )

    wsInternalBroadcastRef ! WsInternalBroadcastActor.Command.Subscribe(webSocketHandlerRef)

    val server: Sink[WsInternalClientHandlerActor.Message, NotUsed] =
      ActorSink
        .actorRef[WsInternalClientHandlerActor.Message](
          ref = webSocketHandlerRef,
          onCompleteMessage = WsInternalClientHandlerActor.Event.Completed(().asRight),
          onFailureMessage = e => WsInternalClientHandlerActor.Event.Completed(e.asLeft)
        )
        .named(s"server-$clientId")

    val serverSink: Sink[Message, NotUsed] =
      mkServerSink[WsPingOrPong, WsInternalClientHandlerActor.Command](clientId, internalClientHandler.healthCheck.pingInterval / 5) {
        case Right(x) => WsInternalClientHandlerActor.Command.ProcessClientMessage(x)
        case Left(x)  => WsInternalClientHandlerActor.Command.ForwardToClient(x)
      }.to(server)

    handleWebSocketMessages { Flow.fromSinkAndSourceCoupled(serverSink, clientSource) }
  }

  // From server to client
  private def mkSource(clientId: String): Source[TextMessage.Strict, typed.ActorRef[WsServerMessage]] =
    ActorSource
      .actorRef[WsServerMessage](
        { case WsServerMessage.Complete => },
        PartialFunction.empty,
        10,
        OverflowStrategy.fail
      )
      .named(s"source-$clientId")
      .map(WsMessage.toStrictTextMessage(_)(WsServerMessage.wsServerMessageWrites))
      .watchTermination()(handleTermination[WsServerMessage])

  // From client to server
  private def mkServerSink[Raw: Reads, T](clientId: String, strictTimeout: FiniteDuration)(f: Either[WsError, Raw] => T) =
    Flow[Message]
      .mapAsync[T](1) {
        case tm: TextMessage =>
          tm.toStrict(strictTimeout)
            .map { message =>
              Json.parse(message.getStrictText).as[Raw].asRight[WsError]
            }
            .recover { case _ => WsError.from(InvalidJson(Nil), time.getTimestamp()).asLeft[Raw] }
            .map(f)

        case bm: BinaryMessage =>
          bm.dataStream.runWith(Sink.ignore)
          Future.failed { new IllegalArgumentException("Binary messages are not supported") }
      }
      .named(s"sink-$clientId")

  private def handleTermination[T](client: typed.ActorRef[T], r: Future[Done]): typed.ActorRef[T] = {
    val cn = client.path.name
    r.onComplete {
      case Success(_) => log.trace(s"[c=$cn] WebSocket connection successfully closed")
      case Failure(e) => log.trace(s"[c=$cn] WebSocket connection closed with an error: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
    }(mat.executionContext)
    client
  }

  // TODO: remove after merge into master
  private def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(error.MatcherIsStarting.toWsHttpResponse(StatusCodes.ServiceUnavailable))
    case Matcher.Status.Stopping => complete(error.MatcherIsStopping.toWsHttpResponse(StatusCodes.ServiceUnavailable))
  }
}
