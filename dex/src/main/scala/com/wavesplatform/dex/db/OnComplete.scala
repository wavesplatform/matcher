package com.wavesplatform.dex.db

import cats.Id

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait OnComplete[F[_]] {

  def onComplete[A, B](fa: F[A])(f: Try[A] => B): Unit

}

object OnComplete {

  def apply[F[_]: OnComplete[*[_]]]: OnComplete[F] = implicitly

  implicit val deriveOnCompleteForFuture: OnComplete[Future] = new OnComplete[Future] {

    override def onComplete[A, B](fa: Future[A])(f: Try[A] => B): Unit =
      fa.onComplete(f)(ExecutionContext.parasitic)

  }

  implicit def deriveOnCompleteForId: OnComplete[Id] = new OnComplete[Id] {

    override def onComplete[A, B](fa: Id[A])(f: Try[A] => B): Unit =
      f(Success(fa))

  }

}
