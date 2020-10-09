package com.wavesplatform.dex.tool.connectors

import akka.actor.ActorSystem
import akka.stream.Materializer
import cats.syntax.either._
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.connection.WsConnectionOps._
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.cli.{lift, ErrorOr}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.tool.connectors.AuthServiceRestConnector.AuthCredentials
import com.wavesplatform.dex.tool.connectors.Connector.RepeatRequestOptions

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

case class DexWsConnector private (target: String, wsc: WsConnection)(implicit system: ActorSystem, materializer: Materializer) extends Connector {

  import DexWsConnector._

  implicit override val repeatRequestOptions: RepeatRequestOptions = RepeatRequestOptions(30, 100.millis)

  private def repeat[A](f: => A)(test: A => Boolean): ErrorOr[A] = repeatRequest(lift(f))(_.exists(test))

  def receiveAtLeastN[T <: WsServerMessage: ClassTag](count: Int): ErrorOr[List[T]] =
    for {
      result <- repeat(wsc.collectMessages[T])(_.length >= count)
      _ <- lift(Thread.sleep(awaitingAdditionalMessagesPeriodMs))
    } yield result

  def receiveInitialMessage: ErrorOr[WsInitial] =
    for {
      wsInitial <- receiveAtLeastN[WsInitial](1).map(_.head)
      _ <- clearMessages()
    } yield wsInitial

  def subscribeForOrderBookUpdates(assetPair: AssetPair): ErrorOr[WsOrderBookChanges] =
    for {
      _ <- lift(wsc.send(WsOrderBookSubscribe(assetPair, defaultDepth)))
      snapshot <- receiveAtLeastN[WsOrderBookChanges](1).bimap(ex => s"Cannot get order book snapshot! $ex", _.head)
    } yield snapshot

  def subscribeForAccountUpdates(credentials: AuthCredentials): ErrorOr[WsAddressChanges] =
    for {
      _ <- lift(wsc.send(WsAddressSubscribe(credentials.keyPair, WsAddressSubscribe.defaultAuthType, credentials.token)))
      snapshot <- receiveAtLeastN[WsAddressChanges](1).bimap(ex => s"Cannot get account snapshot! $ex", _.head)
    } yield snapshot

  def clearMessages(): ErrorOr[Unit] = lift(wsc.clearMessages())

  override def close(): Unit = {
    wsc.close()
    Await.result(wsc.closed.zip(system.terminate()), 3.seconds)
    materializer.shutdown()
  }

}

object DexWsConnector {

  private val defaultDepth = 10
  private val awaitingAdditionalMessagesPeriodMs = 200

  def create(target: String): ErrorOr[DexWsConnector] = {

    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: Materializer = Materializer.matFromSystem(system)

    Try(new WsConnection(target, true)).toEither
      .bimap(ex => s"Web Socket connection cannot be established! $ex", wsc => DexWsConnector(target, wsc))
  }

}
