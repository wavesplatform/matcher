package com.wavesplatform.it.api

import cats.syntax.flatMap._
import com.softwaremill.sttp.{DeserializationError, Response}
import com.wavesplatform.it.api.dex.ThrowableMonadError
import play.api.libs.json.JsError

import scala.concurrent.duration.FiniteDuration

class FOps[F[_]](implicit M: ThrowableMonadError[F], W: CanWait[F]) {
  def repeatUntil[T](f: => F[T], delay: FiniteDuration)(pred: T => Boolean): F[T] = {
    def loop(): F[T] = f.flatMap { x =>
      if (pred(x)) M.pure(x) else W.wait(delay).flatMap(_ => loop())
    }

    loop()
  }

  def repeatUntilResponse[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(
      pred: Response[Either[DeserializationError[JsError], T]] => Boolean): F[T] =
    repeatUntil(f, delay)(pred).flatMap(parseResponse)

  def parseResponse[T](resp: Response[Either[DeserializationError[JsError], T]]): F[T] =
    resp.rawErrorBody match {
      case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
      case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
      case Right(Right(r))    => M.pure(r)
    }
}

object FOps {
  def apply[F[_]: CanWait: ThrowableMonadError] = new FOps[F]
}
