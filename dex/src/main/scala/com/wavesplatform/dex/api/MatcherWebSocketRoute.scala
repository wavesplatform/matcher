package com.wavesplatform.dex.api

import java.nio.charset.StandardCharsets

import akka.actor.ActorRef
import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.directives.FutureDirectives
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, Materializer, OverflowStrategy}
import com.google.common.primitives.Longs
import com.wavesplatform.dex.api.PathMatchers.{AssetPairPM, PublicKeyPM}
import com.wavesplatform.dex.api.http.ApiRoute
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsOrderBook}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.market.MatcherActor
import com.wavesplatform.dex.{AddressActor, AddressDirectory, AssetPairBuilder}
import io.swagger.annotations.Api
import javax.ws.rs.Path

import scala.util.{Failure, Success}

@Path("/ws")
@Api(value = "/web sockets/")
case class MatcherWebSocketRoute(addressDirectory: ActorRef, matcher: ActorRef, assetPairBuilder: AssetPairBuilder)(implicit mat: Materializer)
    extends ApiRoute
    with ScorexLogging {

  private implicit val trm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller

  private val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
    case akka.actor.Status.Success(s: CompletionStrategy) => s
    case akka.actor.Status.Success(_)                     => CompletionStrategy.draining
    case akka.actor.Status.Success                        => CompletionStrategy.draining
  }

  private val failureMatcher: PartialFunction[Any, Throwable] = { case akka.actor.Status.Failure(cause) => cause }

  private def accountUpdatesSource(publicKey: PublicKey): Source[TextMessage.Strict, Unit] =
    Source
      .actorRef[WsAddressState](
        completionMatcher,
        failureMatcher,
        10,
        OverflowStrategy.fail
      )
      .map(wsAddressState => TextMessage.Strict(WsAddressState.format.writes(wsAddressState).toString))
      .mapMaterializedValue { sourceActor =>
        addressDirectory.tell(AddressDirectory.Envelope(publicKey, AddressActor.AddWsSubscription), sourceActor)
      }

  private def orderBookUpdatesSource(pair: AssetPair): Source[TextMessage.Strict, Unit] =
    Source
      .actorRef[WsOrderBook](
        completionMatcher,
        failureMatcher,
        10,
        OverflowStrategy.fail
      )
      .map(wsOrderBookState => TextMessage.Strict(WsOrderBook.wsOrderBookStateFormat.writes(wsOrderBookState).toString))
      .mapMaterializedValue { sourceActor =>
        matcher.tell(MatcherActor.AddWsSubscription(pair), sourceActor)
        sourceActor
      }
      .watchTermination() {
        case (actorRef, r) =>
          r.onComplete {
            case Success(_) =>
              log.trace(s"[${actorRef.hashCode()}] WebSocket connection successfully closed")
            case Failure(e) =>
              log.trace(s"[${actorRef.hashCode()}] WebSocket connection closed with an error: ${Option(e.getMessage).getOrElse(e.getClass.getName)}")
          }(mat.executionContext)
      }

  private def signedGet(prefix: String, publicKey: PublicKey): Directive0 = parameters(('Timestamp, 'Signature)).tflatMap {
    case (timestamp, signature) =>
      Base58
        .tryDecodeWithLimit(signature)
        .map { crypto.verify(_, prefix.getBytes(StandardCharsets.UTF_8) ++ publicKey.arr ++ Longs.toByteArray(timestamp.toLong), publicKey) } match {
        case Success(true) => pass
        case _             => complete(InvalidSignature)
      }
  }

  /** Requires PublicKey, Timestamp and Signature of [prefix `as`, PublicKey, Timestamp] */
  private def accountUpdates: Route = (path("accountUpdates" / PublicKeyPM) & get) { publicKey =>
    signedGet("as", publicKey) {
      handleWebSocketMessages(Flow.fromSinkAndSourceCoupled(Sink.cancelled[Message], accountUpdatesSource(publicKey)))
    }
  }

  private val orderBook: Route = (path("orderbook" / AssetPairPM) & get) { p =>
    // TODO depth
    withAssetPair(p) { pair =>
      handleWebSocketMessages(Flow.fromSinkAndSourceCoupled(Sink.ignore, orderBookUpdatesSource(pair)))
    }
  }

  override def route: Route = pathPrefix("ws") {
    accountUpdates ~ orderBook
  }

  private def withAssetPair(p: AssetPair, formatError: MatcherError => ToResponseMarshallable = InfoNotFound.apply): Directive1[AssetPair] = {
    FutureDirectives.onSuccess { assetPairBuilder.validateAssetPair(p).value } flatMap {
      case Right(_) => provide(p)
      case Left(e)  => complete { formatError(e) }
    }
  }
}
