package com.wavesplatform.it.api

import java.net.InetSocketAddress

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, MonadError}
import com.softwaremill.sttp.playJson.asJson
import com.softwaremill.sttp.{DeserializationError, Response, SttpBackend, MonadError => _, _}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.util.{GlobalTimer, TimerExt}
import com.wavesplatform.transaction
import play.api.libs.json._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait NodeApi[F[_]] {
  def broadcast(tx: transaction.Transaction): F[Unit]
  def waitForTransaction(id: ByteStr): F[Unit]
  def waitForHeightArise(): F[Unit]
}

object NodeApi {
  implicit final class FutureTOps(val self: Future.type) extends AnyVal {
    def repeatUntil[T](f: => Future[T], delay: FiniteDuration)(pred: T => Boolean)(implicit ec: ExecutionContext): Future[T] =
      f.flatMap { x =>
        if (pred(x)) Future.successful(x)
        else GlobalTimer.instance.sleep(delay).flatMap(_ => repeatUntil(f, delay)(pred))
      }
  }

  implicit final class FutureOps[T](val self: Future[T]) extends AnyVal {
    def toUnit(implicit ec: ExecutionContext): Future[Unit] = self.map(_ => ())
  }

  def apply[F[_]](host: => InetSocketAddress)(implicit M: MonadError[F, Throwable], httpBackend: SttpBackend[F, Nothing]): NodeApi[F] =
    new NodeApi[F] {
      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}"

      override def broadcast(tx: transaction.Transaction): F[Unit] = {
        val req = sttp.post(uri"$apiUri/transactions/broadcast").body(Json.stringify(tx.json())).response(asString)
        httpBackend.send(req).map(_ => ())
      }

      override def waitForTransaction(id: ByteStr): F[Unit] = repeatUntil(transactionInfo(id), 1.second)(_.nonEmpty).map(_ => ())

      def transactionInfo(id: ByteStr): F[Option[Transaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/info/$id").response(asJson[Transaction])
        httpBackend.send(req).flatMap { resp =>
          if (resp.code == StatusCodes.NotFound) M.pure(None)
          else
            resp.rawErrorBody match {
              case Left(_)            => M.raiseError[Option[Transaction]](new RuntimeException(s"The server returned an error: ${resp.code}"))
              case Right(Left(error)) => M.raiseError[Option[Transaction]](new RuntimeException(s"Can't parse the response: $error"))
              case Right(Right(r))    => M.pure(Some(r))
            }
        }
      }

      override def waitForHeightArise(): F[Unit] = {
        currentHeight
          .flatMap { origHeight =>
            repeatUntil(currentHeight, 1.second)(_ > origHeight)
          }
          .map(_ => ())
      }

      def currentHeight: F[Int] = {
        val req = sttp.get(uri"$apiUri/blocks/height").response(asJson[HeightResponse])
        httpBackend.send(req).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.raiseError[Int](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(Left(error)) => M.raiseError[Int](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r))    => M.pure(r.height)
          }
        }
      }

      def repeatUntil[T](f: => F[T], delay: FiniteDuration)(pred: T => Boolean): F[T] = {
        def loop(): F[T] = f.flatMap { x =>
          if (pred(x)) M.pure(x) else loop()
        }
        loop()
      }

      def repeatUntilRaw[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(pred: T => Boolean): F[T] = {
        def loop(): F[T] = f.flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r))    => if (pred(r)) M.pure(r) else loop()
          }
        }
        loop()
      }
    }

  def unWrapped(async: NodeApi[Try]): NodeApi[Id] = new NodeApi[Id] {
    override def broadcast(tx: transaction.Transaction): Id[Unit] = async.broadcast(tx).get
    override def waitForTransaction(id: ByteStr): Id[Unit]        = async.waitForTransaction(id).get
    override def waitForHeightArise(): Id[Unit]                   = async.waitForHeightArise().get
  }

  case class HeightResponse(height: Int)
  object HeightResponse {
    implicit val format: Format[HeightResponse] = Json.format[HeightResponse]
  }
}
