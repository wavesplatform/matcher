package com.wavesplatform.it.api

import java.net.InetSocketAddress
import java.util.UUID

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, MonadError}
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{DeserializationError, Response, SttpBackend, MonadError => _, _}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

trait DexApi[F[_]] {
  def place(requestId: UUID, order: Order): F[Unit]
  def waitForOrder(requestId: UUID, id: Order.Id)(pred: Option[MatcherStatusResponse] => Boolean): F[Unit]
  def waitForTransactionsByOrder(requestId: UUID, id: Order.Id)(
      pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]]
}

object DexApi {
  // TODO
  implicit val exchangeTxReads: Reads[exchange.ExchangeTransaction] = Reads { json =>
    JsSuccess(TransactionFactory.fromSignedRequest(json).right.get.asInstanceOf[exchange.ExchangeTransaction])
  }

  implicit val orderWrites: Writes[Order] = Writes(_.json())

  implicit class AssetPairExt(val p: AssetPair) extends AnyVal {
    def toUri: String = s"${AssetPair.assetIdStr(p.amountAsset)}/${AssetPair.assetIdStr(p.priceAsset)}"
  }

  implicit class DexApiOps[F[_]](val self: DexApi[F]) extends AnyVal {
    def waitForOrderStatus(requestId: UUID, id: Order.Id, status: String): F[Unit] =
      self.waitForOrder(requestId, id)(_.map(_.status).contains(status))
    def waitForTransactionsByOrder(requestId: UUID, id: Order.Id, atLeast: Int): F[List[exchange.ExchangeTransaction]] =
      self.waitForTransactionsByOrder(requestId, id)(_.lengthCompare(atLeast) >= 0)
  }

  def apply[F[_]](host: => InetSocketAddress)(implicit M: MonadError[F, Throwable], W: CanWait[F], httpBackend: SttpBackend[F, Nothing]): DexApi[F] =
    new DexApi[F] {
      val defaultAssetPair: AssetPair = AssetPair(IssuedAsset(ByteStr(Array.fill(32)(1))), Waves)

      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}/matcher"

      override def place(requestId: UUID, order: Order): F[Unit] = {
        val req = sttp.post(uri"$apiUri/orderbook").body(order).mapResponse(_ => ()).tag("requestId", requestId)
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap { resp =>
          if (resp.isSuccess) M.pure(())
          else M.raiseError[Unit](new RuntimeException(s"The server returned an error: ${resp.code}"))
        }
      }

      override def waitForOrder(requestId: UUID, id: Order.Id)(pred: Option[MatcherStatusResponse] => Boolean): F[Unit] = {
        val req = sttp.get(uri"$apiUri/orderbook/${defaultAssetPair.toUri}/$id").response(asJson[MatcherStatusResponse]).tag("requestId", requestId)
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.code == StatusCodes.NotFound).flatMap { resp =>
          if (resp.code == StatusCodes.NotFound) M.pure(None)
          else parseResponse(resp).map(Some(_))
        }
      }

      override def waitForTransactionsByOrder(requestId: UUID, id: Order.Id)(
          pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]] =
        repeatUntil[List[exchange.ExchangeTransaction]](transactionsByOrder(requestId, id), 1.second)(pred)

      def transactionsByOrder(requestId: UUID, id: Order.Id): F[List[exchange.ExchangeTransaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/$id").response(asJson[List[exchange.ExchangeTransaction]]).tag("requestId", requestId)
        repeatUntilResponse[List[exchange.ExchangeTransaction]](httpBackend.send(req), 1.second)(_.code == StatusCodes.Ok)
      }

      def repeatUntil[T](f: => F[T], delay: FiniteDuration)(pred: T => Boolean): F[T] = {
        def loop(): F[T] = f.flatMap { x =>
          if (pred(x)) M.pure(x) else W.wait(delay).flatMap(_ => loop())
        }
        loop()
      }

      def repeatUntilResponse[T](f: => F[Response[Either[DeserializationError[JsError], T]]], delay: FiniteDuration)(
          pred: Response[Either[DeserializationError[JsError], T]] => Boolean): F[T] =
        repeatUntil(f, delay)(pred).flatMap(parseResponse)

      def parseResponse[T](resp: Response[Either[DeserializationError[JsError], T]]): F[T] =
        resp.rawErrorBody match {
          case Left(_)            => M.raiseError[T](new RuntimeException(s"The server returned an error: ${resp.code}"))
          case Right(Left(error)) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
          case Right(Right(r))    => M.pure(r)
        }
    }

  def unWrapped(source: DexApi[Try]): DexApi[Id] = new DexApi[Id] {
    override def place(requestId: UUID, order: Order): Id[Unit] = source.place(requestId, order).get
    override def waitForOrder(requestId: UUID, id: Order.Id)(pred: Option[MatcherStatusResponse] => Boolean): Id[Unit] =
      source.waitForOrder(requestId, id)(pred).get
    override def waitForTransactionsByOrder(requestId: UUID, id: Order.Id)(
        pred: List[exchange.ExchangeTransaction] => Boolean): Id[List[exchange.ExchangeTransaction]] =
      source.waitForTransactionsByOrder(requestId, id)(pred).get
  }
}

trait TracedDexApi[F[_]] {
  def place(order: Order)(implicit file: sourcecode.File, line: sourcecode.Line): F[Unit]
  def waitForOrder(id: Order.Id)(pred: Option[MatcherStatusResponse] => Boolean)(implicit file: sourcecode.File, line: sourcecode.Line): F[Unit]
  def waitForTransactionsByOrder(id: Order.Id)(pred: List[exchange.ExchangeTransaction] => Boolean)(
      implicit file: sourcecode.File,
      line: sourcecode.Line): F[List[exchange.ExchangeTransaction]]
  def waitForOrderStatus(id: Order.Id, status: String)(implicit file: sourcecode.File, line: sourcecode.Line): F[Unit]
  def waitForTransactionsByOrder(id: Order.Id, atLeast: Int)(implicit file: sourcecode.File,
                                                             line: sourcecode.Line): F[List[exchange.ExchangeTransaction]]
}

object TracedDexApi {
  def wrap[F[_]](underlying: DexApi[F]): TracedDexApi[F] = new TracedDexApi[F] with ScorexLogging {
    def place(order: Order)(implicit file: sourcecode.File, line: sourcecode.Line): F[Unit] = {
      val requestId = UUID.randomUUID()
      log.trace(s"[$requestId] ${file.value}:${line.value} place($order)")
      underlying.place(requestId, order)
    }

    def waitForOrder(id: Order.Id)(pred: Option[MatcherStatusResponse] => Boolean)(implicit file: sourcecode.File, line: sourcecode.Line): F[Unit] = {
      val requestId = UUID.randomUUID()
      log.trace(s"[$requestId] ${file.value}:${line.value} waitForOrder($id)")
      underlying.waitForOrder(requestId, id)(pred)
    }

    def waitForTransactionsByOrder(id: Order.Id)(pred: List[exchange.ExchangeTransaction] => Boolean)(
        implicit file: sourcecode.File,
        line: sourcecode.Line): F[List[exchange.ExchangeTransaction]] = {
      val requestId = UUID.randomUUID()
      log.trace(s"[$requestId] ${file.value}:${line.value} waitForTransactionsByOrder($id)")
      underlying.waitForTransactionsByOrder(requestId, id)(pred)
    }

    def waitForOrderStatus(id: Order.Id, status: String)(implicit file: sourcecode.File, line: sourcecode.Line): F[Unit] = {
      val requestId = UUID.randomUUID()
      log.trace(s"[$requestId] ${file.value}:${line.value} waitForOrderStatus($id, '$status')")
      underlying.waitForOrder(requestId, id)(_.map(_.status).contains(status))
    }

    def waitForTransactionsByOrder(id: Order.Id, atLeast: Int)(implicit file: sourcecode.File,
                                                               line: sourcecode.Line): F[List[exchange.ExchangeTransaction]] = {
      val requestId = UUID.randomUUID()
      log.trace(s"[$requestId] ${file.value}:${line.value} waitForTransactionsByOrder($id, '$atLeast')")
      underlying.waitForTransactionsByOrder(requestId, id)(_.lengthCompare(atLeast) >= 0)
    }
  }
}
