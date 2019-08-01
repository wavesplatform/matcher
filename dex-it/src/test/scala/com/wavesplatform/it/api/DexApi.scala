package com.wavesplatform.it.api

import java.net.InetSocketAddress

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, MonadError}
import com.softwaremill.sttp.playJson.asJson
import com.softwaremill.sttp.{DeserializationError, Response, SttpBackend, MonadError => _, _}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.assets.exchange
import play.api.libs.json._

import scala.util.Try

trait DexApi[F[_]] {
  def place(order: Order): F[Unit]
  def waitForOrder(id: Order.Id)(pred: MatcherStatusResponse => Boolean): F[Unit]
  def waitForTransactionsByOrder(id: Order.Id)(pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]]
}

object DexApi {
  // TODO
  implicit val exchangeTxReads: Reads[exchange.ExchangeTransaction] = Reads { json =>
    JsSuccess(TransactionFactory.fromSignedRequest(json).right.get.asInstanceOf[exchange.ExchangeTransaction])
  }

  implicit class AssetPairExt(val p: AssetPair) extends AnyVal {
    def toUri: String = s"${AssetPair.assetIdStr(p.amountAsset)}/${AssetPair.assetIdStr(p.priceAsset)}"
  }

  implicit class DexApiOps[F[_]](val self: DexApi[F]) extends AnyVal {
    def waitForOrderStatus(id: Order.Id, status: String): F[Unit] = self.waitForOrder(id)(_.status == status)
    def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[exchange.ExchangeTransaction]] =
      self.waitForTransactionsByOrder(id)(_.lengthCompare(atLeast) >= 0)
  }

  def apply[F[_]](host: => InetSocketAddress)(implicit M: MonadError[F, Throwable], httpBackend: SttpBackend[F, Nothing]): DexApi[F] =
    new DexApi[F] {
      val defaultAssetPair: AssetPair = AssetPair(IssuedAsset(ByteStr(Array.fill(32)(1))), Waves)

      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}/matcher"

      override def place(order: Order): F[Unit] = {
        val req = sttp.post(uri"$apiUri/orderbook").body(order.jsonStr).response(asString)
        httpBackend.send(req).map(_ => ())
      }

      override def waitForOrder(id: Order.Id)(pred: MatcherStatusResponse => Boolean): F[Unit] = {
        val req = sttp.get(uri"$apiUri/orderbook/${defaultAssetPair.toUri}/$id").response(asJson[MatcherStatusResponse])
        repeatUntil[MatcherStatusResponse](httpBackend.send(req))(pred).map(_ => ())
      }

      override def waitForTransactionsByOrder(id: Order.Id)(
          pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/$id").response(asJson[List[exchange.ExchangeTransaction]])
        repeatUntil[List[exchange.ExchangeTransaction]](httpBackend.send(req))(pred)
      }

      def repeatUntil[T](f: => F[Response[Either[DeserializationError[JsError], T]]])(pred: T => Boolean): F[T] = {
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

  def unWrapped(source: DexApi[Try]): DexApi[Id] = new DexApi[Id] {
    override def place(order: Order): Id[Unit]                                                = source.place(order).get
    override def waitForOrder(id: Order.Id)(pred: MatcherStatusResponse => Boolean): Id[Unit] = source.waitForOrder(id)(pred).get
    override def waitForTransactionsByOrder(id: Order.Id)(
        pred: List[exchange.ExchangeTransaction] => Boolean): Id[List[exchange.ExchangeTransaction]] =
      source.waitForTransactionsByOrder(id)(pred).get
  }
}
