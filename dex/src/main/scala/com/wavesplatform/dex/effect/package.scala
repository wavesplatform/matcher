package com.wavesplatform.dex

import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.dex.error.MatcherError

import scala.concurrent.{ExecutionContext, Future}

package object effect {

  type FutureResult[T] = EitherT[Future, MatcherError, T]

  //we need this "def" due to Kamon caching futures problem
  //https://github.com/kamon-io/Kamon/issues/829
  def successAsync: FutureResult[Unit] = liftValueAsync(())

  def liftValueAsync[T](value: T): FutureResult[T] = EitherT(Future.successful(value.asRight[MatcherError]))
  def liftErrorAsync[T](error: MatcherError): FutureResult[T] = EitherT(Future.successful(error.asLeft[T]))
  def liftFutureAsync[T](x: Future[T])(implicit ex: ExecutionContext): FutureResult[T] = EitherT.right[MatcherError](x)
}
