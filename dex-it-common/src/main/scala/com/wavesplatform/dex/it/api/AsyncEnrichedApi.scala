package com.wavesplatform.dex.it.api

import java.net.InetSocketAddress
import java.util.UUID

import sttp.client3._
import com.typesafe.config.Config
import play.api.libs.json.Reads

import scala.concurrent.{ExecutionContext, Future}

abstract class AsyncEnrichedApi[ErrorT: Reads](host: => InetSocketAddress)(implicit
  ec: ExecutionContext,
  httpBackend: SttpBackend[Identity, Any]
) {

  type R[EntityT] = Future[EnrichedResponse[ErrorT, EntityT]]

  def mk[EntityT: Reads](req: Request[Either[String, String], Any]): R[EntityT] = {

    basicRequest
      .response(asString("UTF-8"))
      .send(httpBackend)
      .send(req.tag("requestId", UUID.randomUUID))
      .map(EnrichedResponse(_, new EnrichedResponse.AsJson[ErrorT, EntityT]))
  }

  def mkHocon[EntityT](req: Request[Either[String, String], Any]): R[Config] =
    basicRequest
      .send(req.tag("requestId", UUID.randomUUID))
      .map(EnrichedResponse(_, new EnrichedResponse.AsHocon[ErrorT]))

  def mkIgnore(req: Request[Either[String, String], Any]): R[Unit] =
    basicRequest
      .send(req.tag("requestId", UUID.randomUUID))
      .map(EnrichedResponse(_, new EnrichedResponse.Ignore[ErrorT]))

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

}
