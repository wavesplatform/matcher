package com.wavesplatform.dex.api

import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

import akka.actor.ActorRef
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, Materializer, OverflowStrategy}
import com.google.common.primitives.Longs
import com.wavesplatform.dex.api.PathMatchers.PublicKeyPM
import com.wavesplatform.dex.api.http.ApiRoute
import com.wavesplatform.dex.api.websockets.WsAddressState
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.{AddressActor, AddressDirectory}
import io.swagger.annotations.Api
import javax.ws.rs.Path

import scala.concurrent.duration._
import scala.util.Success

@Path("/ws")
@Api(value = "/web sockets/")
case class MatcherWebSocketRoute(addressDirectory: ActorRef)(implicit mat: Materializer) extends ApiRoute with ScorexLogging {

  private implicit val trm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller

  private def accountUpdatesSource(publicKey: PublicKey) /*: ActorRef*/ = {

    val completionMatcher: PartialFunction[Any, CompletionStrategy] = { case _ => CompletionStrategy.immediately }
    val failureMatcher: PartialFunction[Any, Throwable]             = { case _ => throw new IllegalStateException("Stream processing error") }

    Source
      .actorRef[WsAddressState](
//        completionMatcher = completionMatcher,
//        failureMatcher = failureMatcher,
        bufferSize = 10,
        overflowStrategy = OverflowStrategy.fail
      )
      .map(
        b =>
          TextMessage.Strict(
            b.balances.map { case (asset, balances) => s"$asset: reserved = ${balances.reserved}, tradable = ${balances.tradable}" }.mkString("; ")
        )
      )
      .mapMaterializedValue { sourceActor =>
        addressDirectory.tell(AddressDirectory.Envelope(publicKey, AddressActor.AddWsSubscription), sourceActor)
      }
  }

  private def greeter: Flow[Message, Message, Any] = Flow[Message].mapConcat {
    case tm: TextMessage   => TextMessage(Source.single("Hello ") ++ tm.textStream ++ Source.single("!")) :: Nil
    case bm: BinaryMessage => bm.dataStream.runWith(Sink.ignore); Nil
  }

  private def time: Flow[Message, Message, _] = {

    val sink   = Sink.cancelled[Message]
    val source = Source.tick(1.second, 1.second, "").map(_ => TextMessage(s"Now is ${LocalDateTime.now}"))

    Flow.fromSinkAndSource(sink, source)
  }

  private def signedGet(prefix: String, publicKey: PublicKey): Directive0 = parameters('Timestamp, 'Signature).tflatMap {
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
      handleWebSocketMessages(Flow.fromSinkAndSource(Sink.cancelled[Message], accountUpdatesSource(publicKey)))
    }
  }

  override def route: Route = pathPrefix("ws") {
    accountUpdates ~
      path("greeter") { get { handleWebSocketMessages(greeter) } } ~
      path("time") { get { handleWebSocketMessages(time) } }
  }
}
