package com.wavesplatform.it.api

import java.net.InetSocketAddress

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, MonadError}
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{DeserializationError, Response, SttpBackend, MonadError => _, _}
import com.wavesplatform.api.http.ConnectReq
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.util.{GlobalTimer, TimerExt}
import com.wavesplatform.transaction
import play.api.libs.json._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait NodeApi[F[_]] {
  def connect(toNode: InetSocketAddress): F[Unit]

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

  implicit val transactionWrites: Writes[transaction.Transaction] = Writes[transaction.Transaction](_.json())

  def apply[F[_]](apiKey: String, host: => InetSocketAddress)(implicit M: MonadError[F, Throwable], W: CanWait[F], httpBackend: SttpBackend[F, Nothing]): NodeApi[F] =
    new NodeApi[F] {
      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}"

      override def connect(toNode: InetSocketAddress): F[Unit] = {
        val req = sttp.post(uri"$apiUri/peers/connect").body(ConnectReq(toNode.getHostName, toNode.getPort)).header("X-API-Key", apiKey).mapResponse(_ => ())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          if (resp.code == StatusCodes.Ok) M.pure(())
          else M.raiseError[Unit](new RuntimeException("There is no such method!"))
        }
      }

      override def broadcast(tx: transaction.Transaction): F[Unit] = {
        val req = sttp.post(uri"$apiUri/transactions/broadcast").body(tx).mapResponse(_ => ())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          if (resp.code == StatusCodes.Ok) M.pure(())
          else M.raiseError[Unit](new RuntimeException("There is no such method!"))
        }
      }

      override def waitForTransaction(id: ByteStr): F[Unit] = repeatUntil(transactionInfo(id), 1.second)(_.nonEmpty).map(_ => ())

      def transactionInfo(id: ByteStr): F[Option[Transaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/info/$id").response(asJson[Transaction])
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.pure(None)
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
          if (pred(x)) M.pure(x) else W.wait(delay).flatMap(_ => loop())
        }
        loop()
      }

      def repeatUntilResponse[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(
          pred: Response[Either[DeserializationError[JsError], T]] => Boolean): F[T] =
        repeatUntil(f, delay)(pred).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r))    => M.pure(r)
          }
        }
    }

  def unWrapped(async: NodeApi[Try]): NodeApi[Id] = new NodeApi[Id] {
    override def connect(toNode: InetSocketAddress): Id[Unit]     = async.connect(toNode).get
    override def broadcast(tx: transaction.Transaction): Id[Unit] = async.broadcast(tx).get
    override def waitForTransaction(id: ByteStr): Id[Unit]        = async.waitForTransaction(id).get
    override def waitForHeightArise(): Id[Unit]                   = async.waitForHeightArise().get
  }

  case class HeightResponse(height: Int)
  object HeightResponse {
    implicit val format: Format[HeightResponse] = Json.format[HeightResponse]
  }
}
