package com.wavesplatform.dex

import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}

package object cli {
  type ErrorOr[A] = Either[String, A]

  def lift[A](a: A): ErrorOr[A] = a.asRight

  val success: ErrorOr[Unit] = lift { () }

  def log[F[_]: Applicative](log: String, indent: Option[Int] = None): F[Unit] = Applicative[F].pure {
    // It's fine
    print(indent.foldLeft(log) { (r, indent) =>
      r + " " * (indent - log.length)
    })
  }

  def wrapByLogs[F[_]: Monad, A](begin: String, end: String = "Done\n", indent: Option[Int] = None)(f: => F[A]): F[A] =
    for {
      _      <- log(begin, indent)
      result <- f
      _      <- log(end)
    } yield result
}
