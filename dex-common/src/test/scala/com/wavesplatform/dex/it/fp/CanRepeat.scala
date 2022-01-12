package com.wavesplatform.dex.it.fp

import cats.Id
import cats.implicits.{catsSyntaxEitherId, catsSyntaxFlatMapIdOps}
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.annotation.tailrec

trait CanRepeat[F[_]] {
  def repeatUntil[T](f: => F[T], options: RepeatRequestOptions = RepeatRequestOptions.default)(stopCond: T => Boolean): F[T]
}

object CanRepeat {

  implicit def genericCanRepeat[F[_]](implicit W: CanWait[F], M: ThrowableMonadError[F]): CanRepeat[F] = new CanRepeat[F] {

    override def repeatUntil[T](f: => F[T], options: RepeatRequestOptions = RepeatRequestOptions.default)(stopCond: T => Boolean): F[T] = {
      def safeF: F[Either[Unit, T]] = M.recover(f.map(_.asRight[Unit]))(_ => ().asLeft[T])

      safeF
        .flatMap { firstResp =>
          (firstResp, options).tailRecM[F, (Either[Unit, T], RepeatRequestOptions)] {
            case (resp, currOptions) if currOptions.maxAttempts <= 0 =>
              M.raiseError(new RuntimeException(s"All attempts are out! The last response is: $resp"))

            case (resp, currOptions) =>
              resp match {
                case Right(resp) if stopCond(resp) => M.pure((resp.asRight[Unit], currOptions).asRight)
                case _ => W.wait(options.delayBetweenRequests).productR(safeF).map(x => (x, currOptions.decreaseAttempts).asLeft)
              }
          }
        }
        .flatMap {
          case (Right(x), _) => M.pure(x)
          case r => M.raiseError[T](new RuntimeException(s"Unexpected result: $r"))
        }
    }

  }

  implicit val idCanRepeat: CanRepeat[Id] = new CanRepeat[Id] {

    @tailrec
    override def repeatUntil[T](f: => Id[T], options: RepeatRequestOptions)(stopCond: T => Boolean): Id[T] = {
      val resp =
        try f.asRight[Unit]
        catch {
          case _: Throwable => ().asLeft[T]
        }

      if (options.maxAttempts <= 0) throw new RuntimeException(s"All attempts are out! The last response is: $resp")
      resp match {
        case Right(resp) if stopCond(resp) => resp
        case _ =>
          CanWait[Id].wait(options.delayBetweenRequests)
          repeatUntil(f, options.decreaseAttempts)(stopCond)
      }
    }

  }

}
