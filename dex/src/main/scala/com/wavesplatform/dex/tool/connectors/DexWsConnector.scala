package com.wavesplatform.dex.tool.connectors

import akka.actor.ActorSystem
import akka.stream.Materializer
import cats.syntax.either._
import com.wavesplatform.dex.api.websockets.connection.WsConnection
import com.wavesplatform.dex.api.websockets.connection.WsConnectionOps._
import com.wavesplatform.dex.api.websockets.{WsInitial, WsOrderBook, WsOrderBookSubscribe, WsServerMessage}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.tool._
import com.wavesplatform.dex.tool.connectors.RestConnector.RepeatRequestOptions

import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

case class DexWsConnector private (target: String, wsc: WsConnection) extends Connector {

  import DexWsConnector._

  override val repeatRequestOptions: RestConnector.RepeatRequestOptions = RepeatRequestOptions(30, 100.millis)

  private def repeat[A](f: => A)(test: A => Boolean): ErrorOr[A] = repeatRequest { lift(f) } { _.exists(test) }

  def receiveAtLeastN[T <: WsServerMessage: ClassTag](count: Int): ErrorOr[List[T]] =
    for {
      result <- repeat(wsc.collectMessages[T])(_.length >= count)
      _      <- lift { Thread.sleep(awaitingAdditionalMessagesPeriodMs) }
    } yield result

  def receiveInitialMessage: ErrorOr[WsInitial] =
    for {
      wsInitial <- receiveAtLeastN[WsInitial](1).map(_.head)
      _         <- clearMessages()
    } yield wsInitial

  def subscribeForOrderBookUpdates(assetPair: AssetPair): ErrorOr[WsOrderBook] =
    for {
      _        <- lift { wsc.send(WsOrderBookSubscribe(assetPair, defaultDepth)) }
      snapshot <- receiveAtLeastN[WsOrderBook](1).bimap(ex => s"Cannot get order book snapshot! $ex", _.head)
    } yield snapshot

  def clearMessages(): ErrorOr[Unit] = lift { wsc.clearMessages() }

  override def close(): Unit = wsc.close()
}

object DexWsConnector {

  implicit private val system: ActorSystem        = ActorSystem()
  implicit private val materializer: Materializer = Materializer.matFromSystem(system)

  private val defaultDepth                       = 10
  private val awaitingAdditionalMessagesPeriodMs = 200

  def create(target: String): ErrorOr[DexWsConnector] =
    Try { new WsConnection(target, true) }.toEither
      .leftMap(ex => s"Web Socket connection cannot be established! $ex")
      .map(wsc => DexWsConnector(target, wsc))
}
