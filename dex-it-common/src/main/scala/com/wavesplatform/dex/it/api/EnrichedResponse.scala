package com.wavesplatform.dex.it.api

import cats.implicits.catsSyntaxEitherId
import com.softwaremill.sttp.Response
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import play.api.libs.json.{Json, Reads}

sealed trait EnrichedResponse[T] {
  def response: Response[String]
  def tryGet: Either[MatcherError, T]
}

object EnrichedResponse {
  case class AsJson[T](response: Response[String])(implicit val reads: Reads[T]) extends EnrichedResponse[T] {
    override def tryGet: Either[MatcherError, T] = response.body match {
      case Left(e) =>
        Json.parse(e)
          .asOpt[MatcherError]
          .fold(throw new RuntimeException(s"The server returned error, but can't parse response as MatcherError: $e"))(identity)
          .asLeft
      case Right(x) =>
        Json.parse(x)
          .asOpt(reads)
          .fold(throw new RuntimeException(s"The server returned success, but can't parse response: $x"))(identity)
          .asRight
    }
  }

  case class AsHocon[T](response: Response[String]) extends EnrichedResponse[T] {
    override def tryGet: Either[MatcherError, T] = ???
  }
}
