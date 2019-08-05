package com.wavesplatform.it.api

import cats.MonadError
import cats.syntax.flatMap._
import com.softwaremill.sttp.{DeserializationError, Response, MonadError => _}
import play.api.libs.json.JsError

import scala.concurrent.duration.FiniteDuration

object Implicits {
  // should be lazy
//  implicit final class Ops[F[_]](val self: F[_])(implicit M: MonadError[F, Throwable]) {
//    def repeatUntil[T](delay: FiniteDuration)(pred: T => Boolean): F[T] = {
//      def loop(): F[T] = self.flatMap { x =>
//        if (pred(x)) M.pure(x) else loop()
//      }
//      loop()
//    }
//
//    // todo
//    def repeatUntilRaw[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(pred: T => Boolean): F[T] = {
//      def loop(): F[T] = f.flatMap { resp =>
//        resp.rawErrorBody match {
//          case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
//          case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
//          case Right(Right(r))    => if (pred(r)) M.pure(r) else loop()
//        }
//      }
//      loop()
//    }
//
//    def handleSerDeErrors[T](f: F[Response[Either[DeserializationError[JsError], T]]]): F[T] =
//      f.flatMap { resp =>
//        resp.rawErrorBody match {
//          case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
//          case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
//          case Right(Right(r))    => M.pure(r)
//        }
//      }
//  }
}
