package com.wavesplatform.dex.it.api

import java.net.InetSocketAddress
import java.util.UUID

import com.softwaremill.sttp.{Request, SttpBackend}
import play.api.libs.json.Reads

import scala.concurrent.{ExecutionContext, Future}

abstract class AsyncEnrichedApi(host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing]) {

  def mk[T: Reads](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map {
      EnrichedResponse.AsJson[T](_)
    }

  def mkHocon[T](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map(EnrichedResponse.AsHocon[T])

  def mkIgnore(req: Request[String, Nothing]): AsyncEnriched[Unit] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map(EnrichedResponse.Ignore)

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

}
