package com.wavesplatform.it.api

import java.net.InetSocketAddress

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, MonadError}
import com.softwaremill.sttp.playJson.asJson
import com.softwaremill.sttp.{DeserializationError, Response, SttpBackend, MonadError => _, _}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import play.api.libs.json.JsError

import scala.util.Try

trait DexApi[F[_]] {
  def place(order: Order): F[Unit]
  def waitForOrder(id: Order.Id)(pred: MatcherStatusResponse => Boolean): F[Unit]
  def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]]
}

object DexApi {
  implicit class AssetPairExt(val p: AssetPair) extends AnyVal {
    def toUri: String = s"${AssetPair.assetIdStr(p.amountAsset)}/${AssetPair.assetIdStr(p.priceAsset)}"
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

      override def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/$id").response(asJson[List[ExchangeTransaction]])
        repeatUntil[List[ExchangeTransaction]](httpBackend.send(req))(pred)
      }

      def repeatUntil[T](f: => F[Response[Either[DeserializationError[JsError], T]]])(pred: T => Boolean): F[T] = {
        def loop(): F[T] = f.flatMap {
          _.rawErrorBody match {
            case Left(error) => M.raiseError[T](new RuntimeException(s"Can't parse the response: $error"))
            case Right(r)    => if (pred(r)) M.pure(()) else loop()
          }
        }
        loop()
      }
    }

  def unWrapped(source: DexApi[Try]): DexApi[Id] = new DexApi[Id] {
    override def place(order: Order): Id[Unit]                                                = source.place(order).get
    override def waitForOrder(id: Order.Id)(pred: MatcherStatusResponse => Boolean): Id[Unit] = source.waitForOrder(id)(pred).get
    override def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): Id[List[ExchangeTransaction]] =
      source.waitForTransactionsByOrder(id)(pred).get
  }
}
