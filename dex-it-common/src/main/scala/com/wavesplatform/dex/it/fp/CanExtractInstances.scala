package com.wavesplatform.dex.it.fp

import cats.{Id, MonadError}

trait CanExtractInstances extends {
  implicit def monadError[F[_]](implicit M: MonadError[F, Throwable]): CanExtract[F] = new CanExtract[F] {
    override def extract[ErrorT, ResultT](f: => F[Either[ErrorT, ResultT]]): F[ResultT] = M.flatMap(f) {
      case Left(e)  => M.raiseError(new RuntimeException(s"Can't extract: $e"))
      case Right(r) => M.pure(r)
    }
  }

  implicit val id: CanExtract[Id] = new CanExtract[Id] {
    override def extract[ErrorT, ResultT](f: => Id[Either[ErrorT, ResultT]]): Id[ResultT] = f match {
      case Left(e)  => throw new RuntimeException(s"Can't extract: $e")
      case Right(r) => r
    }
  }
}
