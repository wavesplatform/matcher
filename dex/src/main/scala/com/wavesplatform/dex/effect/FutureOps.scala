package com.wavesplatform.dex.effect

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

object FutureOps {

  implicit final class Implicits(val self: Future.type) extends AnyVal {

    def inSeries[A, B](xs: Iterable[A])(f: A => Future[B])(implicit ec: ExecutionContext): Future[Queue[B]] =
      xs.foldLeft(Future.successful(Queue.empty[B])) {
        case (r, x) =>
          for {
            xs <- r
            b <- f(x)
          } yield xs.enqueue(b)
      }

  }

}
