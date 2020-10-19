package com.wavesplatform.dex.it.api

import cats.implicits.catsSyntaxEitherId
import com.softwaremill.sttp.Response
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.EnrichedResponse.As
import play.api.libs.json.{Json, Reads}

case class EnrichedResponse[ErrorT, EntityT](response: Response[String], as: As[ErrorT, EntityT]) {
  def tryGet: Either[ErrorT, EntityT] = as.tryGet(response)

  def unsafeGet: EntityT = tryGet match {
    case Left(e) => throw new RuntimeException(s"An unexpected error: $e")
    case Right(x) => x
  }

}

object EnrichedResponse {

  sealed trait As[ErrorT, EntityT] {
    def tryGet(response: Response[String]): Either[ErrorT, EntityT]
  }

  class AsJson[ErrorT: Reads, EntityT: Reads] extends As[ErrorT, EntityT] {

    override def tryGet(response: Response[String]): Either[ErrorT, EntityT] = parseWithError(response) { x =>
      Json.parse(x)
        .asOpt[EntityT]
        .fold(throw new RuntimeException(s"The server returned success, but can't parse response: $x"))(identity)
        .asRight[ErrorT]
    }

  }

  class AsHocon[ErrorT: Reads] extends As[ErrorT, Config] {

    override def tryGet(response: Response[String]): Either[ErrorT, Config] = parseWithError(response) { x =>
      ConfigFactory.parseString(x).asRight
    }

  }

  class Ignore[ErrorT: Reads] extends As[ErrorT, Unit] {
    override def tryGet(response: Response[String]): Either[ErrorT, Unit] = parseWithError(response)(_ => ().asRight)
  }

  def parseWithError[ErrorT: Reads, EntityT](response: Response[String])(f: String => Either[ErrorT, EntityT]): Either[ErrorT, EntityT] =
    response.body match {
      case Right(x) => f(x)
      case Left(e) =>
        Json.parse(e)
          .asOpt[ErrorT]
          .fold(throw new RuntimeException(s"The server returned error, but can't parse response as error: $e"))(identity)
          .asLeft
    }

}
