package com.wavesplatform.dex.it.api

import java.net.InetSocketAddress
import java.util.UUID

import com.softwaremill.sttp.{Request, SttpBackend}
import com.typesafe.config.Config
import play.api.libs.json.Reads

import scala.concurrent.{ExecutionContext, Future}

abstract class AsyncEnrichedApi[ErrorT: Reads](host: => InetSocketAddress)(implicit
  ec: ExecutionContext,
  httpBackend: SttpBackend[Future, Nothing]
) {

  type R[EntityT] = Future[EnrichedResponse[ErrorT, EntityT]]

  def mk[EntityT: Reads](req: Request[String, Nothing]): R[EntityT] =
    httpBackend
      .send[String](req.tag("requestId", UUID.randomUUID))
      .map(EnrichedResponse(_, new EnrichedResponse.AsJson[ErrorT, EntityT]))

  def mkHocon[EntityT](req: Request[String, Nothing]): R[Config] =
    httpBackend
      .send[String](req.tag("requestId", UUID.randomUUID))
      .map(EnrichedResponse(_, new EnrichedResponse.AsHocon[ErrorT]))

  def mkIgnore(req: Request[String, Nothing]): R[Unit] =
    httpBackend
      .send[String](req.tag("requestId", UUID.randomUUID))
      .map(EnrichedResponse(_, new EnrichedResponse.Ignore[ErrorT]))

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

}
