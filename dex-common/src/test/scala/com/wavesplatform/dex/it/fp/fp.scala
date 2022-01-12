package com.wavesplatform.dex.it

import cats.arrow.FunctionK
import cats.tagless.FunctorK
import cats.{~>, Id, MonadError}

import scala.util.Try

package object fp {

  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  implicit val tryToId: Try ~> Id = new FunctionK[Try, Id] {
    def apply[A](fa: Try[A]): Id[A] = fa.get
  }

  def sync[F[_[_]]](x: F[Try])(implicit functorK: FunctorK[F]): F[Id] = functorK.mapK(x)(tryToId)
}
