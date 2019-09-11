package com.wavesplatform.it.api

import java.nio.charset.StandardCharsets

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.softwaremill.sttp.{DeserializationError, Response}
import com.wavesplatform.it.api.dex.ThrowableMonadError
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

case class RepeatRequestOptions(delayBetweenRequests: FiniteDuration, maxAttempts: Int) {
  def decreaseAttempts: RepeatRequestOptions = copy(maxAttempts = maxAttempts - 1)
}

class FOps[F[_]](implicit M: ThrowableMonadError[F], W: CanWait[F]) {
  def repeatUntil[T](f: => F[T], options: RepeatRequestOptions)(stopCond : T => Boolean): F[T] =
    f.flatMap { firstResp =>
        (firstResp, options).tailRecM[F, (T, RepeatRequestOptions)] {
          case (resp, currOptions) =>
            if (stopCond(resp)) M.pure((resp, currOptions).asRight)
            else if (currOptions.maxAttempts <= 0) M.raiseError(new RuntimeException("All attempts are out!"))
            else W.wait(options.delayBetweenRequests).productR(f).map(x => (x, currOptions.decreaseAttempts).asLeft)
        }
      }
      .map(_._1)

  def repeatUntil[T](f: => F[T], delay: FiniteDuration)(pred: T => Boolean): F[T] =
    f.flatMap {
      _.tailRecM[F, T] { x =>
        if (pred(x)) M.pure(x.asRight)
        else W.wait(delay).productR(f).map(_.asLeft)
      }
    }

  def repeatUntilResponse[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(
      pred: Response[Either[DeserializationError[JsError], T]] => Boolean): F[T] =
    repeatUntil(f, delay)(pred).flatMap(parseResponse)

  def parseResponse[T](resp: Response[Either[DeserializationError[JsError], T]]): F[T] =
    resp.rawErrorBody match {
      case Left(e) =>
        M.raiseError[T](
          new RuntimeException(s"The server returned an error. HTTP code is ${resp.code}, body: ${new String(e, StandardCharsets.UTF_8)}"))
      case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
      case Right(Right(r))    => M.pure(r)
    }

  def parseTryResponse[E: Reads, T](resp: Response[T]): F[Either[E, T]] = resp.rawErrorBody match {
    case Right(r) => M.pure(Right(r))
    case Left(bytes) =>
      try Json.parse(bytes).validate[E] match {
        case JsSuccess(x, _) => M.pure(Left(x))
        case JsError(e) =>
          M.raiseError[Either[E, T]](
            new RuntimeException(s"The server returned an error: ${resp.code}, also can't parse as MatcherError:\n${e.mkString("\n")}"))
      } catch {
        case NonFatal(e) =>
          M.raiseError[Either[E, T]](new RuntimeException(s"The server returned an error: ${resp.code}, also can't parse as MatcherError", e))
      }
  }

  def parseTryResponseEither[E: Reads, T](resp: Response[Either[DeserializationError[JsError], T]]): F[Either[E, T]] = resp.rawErrorBody match {
    case Right(Right(r)) => M.pure(Right(r))
    case Right(Left(e))  => M.raiseError[Either[E, T]](new RuntimeException(s"The server returned success, but can't parse response: $e"))
    case Left(bytes) =>
      try Json.parse(bytes).validate[E] match {
        case JsSuccess(x, _) => M.pure(Left(x))
        case JsError(e) =>
          M.raiseError[Either[E, T]](
            new RuntimeException(s"The server returned an error: ${resp.code}, also can't parse as MatcherError:\n${e.mkString("\n")}"))
      } catch {
        case NonFatal(e) =>
          M.raiseError[Either[E, T]](new RuntimeException(s"The server returned an error: ${resp.code}, also can't parse as MatcherError", e))
      }
  }
}

object FOps {
  def apply[F[_]: CanWait: ThrowableMonadError] = new FOps[F]
}
