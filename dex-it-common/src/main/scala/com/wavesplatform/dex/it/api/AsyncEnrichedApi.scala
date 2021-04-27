package com.wavesplatform.dex.it.api

import java.net.InetSocketAddress
import sttp.client3._
import com.typesafe.config.Config
import kamon.trace.Identifier
import play.api.libs.json.Reads

import scala.concurrent.{ExecutionContext, Future}

abstract class AsyncEnrichedApi[ErrorT: Reads](host: => InetSocketAddress)(implicit
  ec: ExecutionContext,
  httpBackend: SttpBackend[Future, Any]
) {

  type R[EntityT] = Future[EnrichedResponse[ErrorT, EntityT]]

  def mk[EntityT: Reads](req: Request[Either[String, String], Any]): R[EntityT] = {
    val traceId = generateTraceId()
    req
      .tag("traceId", traceId)
      .header("x-trace-id", traceId)
      .contentType("application/json")
      .send(httpBackend)
      .map(EnrichedResponse(_, new EnrichedResponse.AsJson[ErrorT, EntityT]))
  }

  def mkHocon[EntityT](req: Request[Either[String, String], Any]): R[Config] = {
    val traceId = generateTraceId()
    req
      .tag("traceId", traceId)
      .header("x-trace-id", traceId)
      .send(httpBackend)
      .map(EnrichedResponse(_, new EnrichedResponse.AsHocon[ErrorT]))
  }

  def mkIgnore(req: Request[Either[String, String], Any]): R[Unit] = {
    val traceId = generateTraceId()
    req
      .tag("traceId", traceId)
      .header("x-trace-id", traceId)
      .send(httpBackend)
      .map(EnrichedResponse(_, new EnrichedResponse.Ignore[ErrorT]))
  }

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

  private def generateTraceId(): String =
    Identifier.Factory.EightBytesIdentifier.generate().string

}
