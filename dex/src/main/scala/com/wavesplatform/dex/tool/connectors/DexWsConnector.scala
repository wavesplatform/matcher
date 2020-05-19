package com.wavesplatform.dex.tool.connectors

import akka.actor.ActorSystem
import akka.stream.Materializer
import cats.syntax.either._
import com.wavesplatform.dex.api.websockets.WsInitial
import com.wavesplatform.dex.api.websockets.connection.WsConnection
import com.wavesplatform.dex.api.websockets.connection.WsConnectionOps._
import com.wavesplatform.dex.tool.connectors.RestConnector.RepeatRequestOptions
import com.wavesplatform.dex.tool.{ErrorOr, _}

import scala.concurrent.duration._
import scala.util.Try

case class DexWsConnector private (target: String, wsc: WsConnection) extends Connector {

  override val repeatRequestOptions: RestConnector.RepeatRequestOptions = RepeatRequestOptions(10, 100.millis)

  def receiveInitialMessage: ErrorOr[WsInitial] = repeatRequest { lift(wsc.collectMessages[WsInitial]) } { _.exists(_.nonEmpty) }.map(_.head)

  override def close(): Unit = wsc.close()
}

object DexWsConnector {

  implicit private val system: ActorSystem        = ActorSystem()
  implicit private val materializer: Materializer = Materializer.matFromSystem(system)

  def create(target: String): ErrorOr[DexWsConnector] =
    Try { new WsConnection(target, true) }.toEither
      .leftMap(ex => s"Web Socket connection cannot be established! $ex")
      .map(wsc => DexWsConnector(target, wsc))
}
