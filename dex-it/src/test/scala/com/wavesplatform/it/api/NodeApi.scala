package com.wavesplatform.it.api

import java.net.{InetSocketAddress, SocketException}

import cats.MonadError
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.tagless.{Derive, FunctorK}
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{Response, SttpBackend, MonadError => _, _}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.http.ConnectReq
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.{account, transaction}
import play.api.libs.json._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try
import scala.util.control.NonFatal

case class ConnectedPeersResponse(peers: List[PeerInfo])
object ConnectedPeersResponse {
  implicit val format: Format[ConnectedPeersResponse] = Json.format[ConnectedPeersResponse]
}

case class PeerInfo(address: String, declaredAddress: String, peerName: String, peerNonce: Long, applicationName: String, applicationVersion: String)
object PeerInfo {
  implicit val format: Format[PeerInfo] = Json.format[PeerInfo]
}

trait NodeApi[F[_]] extends HasWaitReady[F] {
  def balance(address: com.wavesplatform.account.Address, asset: Asset): F[Long]

  def connect(toNode: InetSocketAddress): F[Unit]
  def connected: F[ConnectedPeersResponse]
  def waitForConnectedPeer(toNode: InetSocketAddress): F[Unit]

  def broadcast(tx: transaction.Transaction): F[Unit]
  def transactionInfo(id: ByteStr): F[Option[Transaction]]
  def rawTransactionInfo(id: ByteStr): F[Option[JsValue]]
  def waitForTransaction(id: ByteStr): F[Unit]
  def waitForHeightArise(): F[Unit]

  def config: F[Config]
}

object NodeApi {
  implicit val functorK: FunctorK[NodeApi] = Derive.functorK[NodeApi]

  implicit val transactionWrites: Writes[transaction.Transaction] = Writes[transaction.Transaction](_.json())

  def apply[F[_]](apiKey: String,
                  host: => InetSocketAddress)(implicit M: MonadError[F, Throwable], W: CanWait[F], httpBackend: SttpBackend[F, Nothing]): NodeApi[F] =
    new NodeApi[F] {
      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}"

      override def balance(address: account.Address, asset: Asset): F[Long] = asset match {
        case asset: IssuedAsset =>
          val req = sttp.get(uri"$apiUri/assets/balance/$address/${AssetPair.assetIdStr(asset)}").response(asJson[AssetBalance])
          httpBackend
            .send(req)
            .flatMap { resp =>
              resp.rawErrorBody match {
                case Left(_)            => M.raiseError[AssetBalance](new RuntimeException(s"The server returned an error: ${resp.code}"))
                case Right(Left(error)) => M.raiseError[AssetBalance](new RuntimeException(s"Can't parse the response: $error"))
                case Right(Right(r))    => M.pure(r)
              }
            }
            .map(_.balance)

        case Waves =>
          val req = sttp.get(uri"$apiUri/addresses/balance/$address").response(asJson[Balance])
          httpBackend
            .send(req)
            .flatMap { resp =>
              resp.rawErrorBody match {
                case Left(_)            => M.raiseError[Balance](new RuntimeException(s"The server returned an error: ${resp.code}"))
                case Right(Left(error)) => M.raiseError[Balance](new RuntimeException(s"Can't parse the response: $error"))
                case Right(Right(r))    => M.pure(r)
              }
            }
            .map(_.balance)
      }

      override def connect(toNode: InetSocketAddress): F[Unit] = {
        val req =
          sttp.post(uri"$apiUri/peers/connect").body(ConnectReq(toNode.getHostName, toNode.getPort)).header("X-API-Key", apiKey).mapResponse(_ => ())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          if (resp.code == StatusCodes.Ok) M.pure(())
          else M.raiseError[Unit](new RuntimeException("There is no such method!"))
        }
      }

      override def connected: F[ConnectedPeersResponse] = {
        val req = sttp.get(uri"$apiUri/peers/connected").response(asJson[ConnectedPeersResponse])
        repeatUntilAttempts(httpBackend.send(req), 1.second, 10)(resp => resp.code == StatusCodes.Ok).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.raiseError[ConnectedPeersResponse](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(Left(error)) => M.raiseError[ConnectedPeersResponse](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r))    => M.pure(r)
          }
        }
      }

      override def waitForConnectedPeer(toNode: InetSocketAddress): F[Unit] = {
        val hostName = toNode.getHostName
        repeatUntil(connected, 1.second)(_.peers.exists(p => p.address.contains(hostName))).map(_ => ())
      }

      override def broadcast(tx: transaction.Transaction): F[Unit] = {
        val req = sttp.post(uri"$apiUri/transactions/broadcast").body(tx).mapResponse(_ => ())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          if (resp.code == StatusCodes.Ok) M.pure(())
          else M.raiseError[Unit](new RuntimeException("There is no such method!"))
        }
      }

      override def transactionInfo(id: ByteStr): F[Option[Transaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/info/$id").response(asJson[Transaction])
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.pure(None)
            case Right(Left(error)) => M.raiseError[Option[Transaction]](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r))    => M.pure(Some(r))
          }
        }
      }

      override def rawTransactionInfo(id: ByteStr): F[Option[JsValue]] = {
        val req = sttp.get(uri"$apiUri/transactions/info/$id").response(asJson[JsValue])
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.code == StatusCodes.Ok || resp.code == StatusCodes.NotFound).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)            => M.pure(None)
            case Right(Left(error)) => M.raiseError[Option[JsValue]](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r))    => M.pure(Some(r))
          }
        }
      }

      override def waitForTransaction(id: ByteStr): F[Unit] = repeatUntil(transactionInfo(id), 1.second)(_.nonEmpty).map(_ => ())

      override def waitForHeightArise(): F[Unit] = {
        currentHeight
          .flatMap { origHeight =>
            repeatUntil(currentHeight, 1.second)(_ > origHeight)
          }
          .map(_ => ())
      }

      override def config: F[Config] = {
        val req = sttp.get(uri"$apiUri/blocks/height").response(asString("UTF-8"))
        httpBackend.send(req).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_)        => M.raiseError[Config](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(content) => M.fromTry(Try(ConfigFactory.parseString(content)))
          }
        }
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

      override def waitReady: F[Unit] = {
        val req = sttp.get(uri"$apiUri/blocks/height").mapResponse(_ => ())

        def loop(): F[Response[Unit]] = M.handleErrorWith(httpBackend.send(req)) {
          case _: SocketException => W.wait(1.second).flatMap(_ => loop())
          case NonFatal(e)        => M.raiseError(e)
        }

        repeatUntil(loop(), 1.second)(_.code == StatusCodes.Ok).map(_ => ())
      }

      def repeatUntil[T](f: => F[T], delay: FiniteDuration)(pred: T => Boolean): F[T] =
        f.flatMap {
          _.tailRecM[F, T] { x =>
            if (pred(x)) M.pure(x.asRight)
            else W.wait(delay).productR(f).map(_.asLeft)
          }
        }

      def repeatUntilAttempts[T](f: => F[T], delay: FiniteDuration, maxAttempts: Int)(pred: T => Boolean): F[T] =
        f.flatMap { x =>
          (x, maxAttempts - 1).tailRecM[F, T] {
            case (x, restAttempts) =>
              if (pred(x)) M.pure(x.asRight)
              else if (restAttempts == 0) M.raiseError(new RuntimeException("All attempts are out"))
              else W.wait(delay).productR(f).map(x => (x, restAttempts - 1).asLeft)
          }
        }

//      def repeatUntilResponse[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(
//          pred: Response[Either[DeserializationError[JsError], T]] => Boolean): F[T] =
//        repeatUntil(f, delay)(pred).flatMap { resp =>
//          resp.rawErrorBody match {
//            case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
//            case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
//            case Right(Right(r))    => M.pure(r)
//          }
//        }
    }

  case class HeightResponse(height: Int)
  object HeightResponse {
    implicit val format: Format[HeightResponse] = Json.format[HeightResponse]
  }
}
