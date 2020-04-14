package com.wavesplatform.dex.it.sttp

import java.util.UUID

import cats.syntax.flatMap._
import com.softwaremill.sttp.playJson.asJson
import com.softwaremill.sttp.{DeserializationError, Id, RequestT, SttpBackend}
import com.wavesplatform.dex.it.fp.{CanWait, FOps, ThrowableMonadError}
import play.api.libs.json.{JsError, Reads}

class SttpBackendOps[F[_]: CanWait: ThrowableMonadError, ErrorT: Reads](implicit httpBackend: SttpBackend[F, Nothing]) {

  private val ops = FOps[F]; import ops._

  def tryParse[ResultT](req: RequestT[Id, Either[DeserializationError[JsError], ResultT], Nothing]): F[Either[ErrorT, ResultT]] =
    httpBackend.send(req.tag("requestId", UUID.randomUUID)).flatMap(parseTryResponseEither[ErrorT, ResultT])

  def tryParseJson[ResultT: Reads](req: RequestT[Id, String, Nothing]): F[Either[ErrorT, ResultT]] =
    httpBackend.send(req.response(asJson[ResultT]).tag("requestId", UUID.randomUUID)).flatMap(parseTryResponseEither[ErrorT, ResultT])

  def tryUnit(req: RequestT[Id, String, Nothing]): F[Either[ErrorT, Unit]] =
    httpBackend.send(req.mapResponse(_ => ()).tag("requestId", UUID.randomUUID)).flatMap(parseTryResponse[ErrorT, Unit])
}

object SttpBackendOps {
  def apply[F[_]: CanWait: ThrowableMonadError, ErrorT: Reads](implicit httpBackend: SttpBackend[F, Nothing]): SttpBackendOps[F, ErrorT] =
    new SttpBackendOps[F, ErrorT]
}
