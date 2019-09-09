package com.wavesplatform.it.api

import java.net.{InetSocketAddress, SocketException}
import java.util.UUID

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.tagless.{Derive, FunctorK}
import com.google.common.primitives.Longs
import com.softwaremill.sttp.Uri.QueryFragment
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{Id, Response, SttpBackend, MonadError => _, _}
import com.typesafe.config.Config
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.dex.api.CancelOrderRequest
import com.wavesplatform.it.api.OrderBookHistoryItem.byteStrFormat
import com.wavesplatform.it.api.dex.ThrowableMonadError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.{Asset, TransactionFactory}
import play.api.libs.json._
import shapeless.ops.hlist.{Mapper, ToTraversable, Zip}
import shapeless.{Generic, HList, HNil, _}

import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

case class MatcherError(error: Int, message: String, status: String, params: Option[MatcherError.Params])
object MatcherError {

  implicit val format: Format[MatcherError] = Json.format[MatcherError]

  case class Params(assetId: Option[String] = None, address: Option[String] = None) {
    def isEmpty: Boolean = assetId.isEmpty && address.isEmpty
  }

  object Params {

    implicit val format: Format[Params] = Json.format[Params]

    private object containsPoly extends Poly1 {
      implicit def contains[T: Ordering] = at[(Option[T], Option[T])] {
        case (Some(l), Some(r)) => l == r
        case (None, None)       => true
        case _                  => false
      }
    }

    private def internalContains[HParams <: HList, ZHParams <: HList, Booleans <: HList](obj: Params, part: Params)(
        implicit gen: Generic.Aux[Params, HParams],
        zip: Zip.Aux[HParams :: HParams :: HNil, ZHParams],
        mapper: Mapper.Aux[containsPoly.type, ZHParams, Booleans],
        toList: ToTraversable.Aux[Booleans, List, Boolean]): Boolean =
      gen.to(obj).zip(gen.to(part))(zip).map(containsPoly).toList[Boolean](toList).forall(identity)

    def contains(obj: Params, part: Params): Boolean = internalContains(obj, part)
  }
}

case class OrderStatusResponse(status: OrderStatus, filledAmount: Option[Long])
object OrderStatusResponse {
  implicit val format: Format[OrderStatusResponse] = Json.format[OrderStatusResponse]
}

trait DexApi[F[_]] extends HasWaitReady[F] {

  def publicKey: F[PublicKey]

  def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Map[Asset, Long]]
  def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Map[Asset, Long]]

  def tryPlace(order: Order): F[Either[MatcherError, Unit]]

  def place(order: Order): F[MatcherResponse]
  def placeMarket(order: Order): F[MatcherResponse]

  def tryCancel(owner: KeyPair, order: Order): F[Either[MatcherError, MatcherStatusResponse]] = tryCancel(owner, order.assetPair, order.id())
  def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, MatcherStatusResponse]]

  def cancel(owner: KeyPair, order: Order): F[MatcherStatusResponse] = cancel(owner, order.assetPair, order.id())
  def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[MatcherStatusResponse]

  // TODO should handle MatcherError
  def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Unit]
  def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Unit]

  def cancelWithApiKey(id: Order.Id): F[MatcherStatusResponse]

  def tryOrderStatus(order: Order): F[Either[MatcherError, OrderStatusResponse]] = tryOrderStatus(order.assetPair, order.id())
  def tryOrderStatus(assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, OrderStatusResponse]]

  def orderStatus(order: Order): F[OrderStatusResponse] = orderStatus(order.assetPair, order.id())
  def orderStatus(assetPair: AssetPair, id: Order.Id): F[OrderStatusResponse]

  def transactionsByOrder(id: Order.Id): F[List[exchange.ExchangeTransaction]]

  def orderHistory(owner: KeyPair, activeOnly: Option[Boolean] = None, timestamp: Long = System.currentTimeMillis()): F[List[OrderBookHistoryItem]]
  def orderHistoryWithApiKey(owner: com.wavesplatform.account.Address, activeOnly: Option[Boolean] = None): F[List[OrderBookHistoryItem]]

  def orderHistoryByPair(owner: KeyPair,
                         assetPair: AssetPair,
                         activeOnly: Option[Boolean] = None,
                         timestamp: Long = System.currentTimeMillis()): F[List[OrderBookHistoryItem]]

  def allOrderBooks: F[MarketDataInfo]

  def tryOrderBook(assetPair: AssetPair): F[Either[MatcherError, OrderBookResponse]]

  def orderBook(assetPair: AssetPair): F[OrderBookResponse]

  def tryOrderBookStatus(assetPair: AssetPair): F[Either[MatcherError, MarketStatusResponse]]
  def orderBookStatus(assetPair: AssetPair): F[MarketStatusResponse]

  def tryDeleteOrderBook(assetPair: AssetPair): F[Either[MatcherError, Unit]]

  def tryUpsertRate(asset: Asset, rate: Double): F[Either[MatcherError, (StatusCode, RatesResponse)]]
  def upsertRate(asset: Asset, rate: Double): F[(StatusCode, RatesResponse)]

  def tryDeleteRate(asset: Asset): F[Either[MatcherError, RatesResponse]]
  def deleteRate(asset: Asset): F[RatesResponse]

  def rates: F[Map[Asset, Double]]

  def currentOffset: F[Long]
  def lastOffset: F[Long]
  def allSnapshotOffsets: F[Map[String, Long]] // TODO Map[AssetPair, Long]

  // TODO move

  def waitForOrder(order: Order)(pred: OrderStatusResponse => Boolean): F[OrderStatusResponse] = waitForOrder(order.assetPair, order.id())(pred)
  def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: OrderStatusResponse => Boolean): F[OrderStatusResponse]

  def waitForOrderStatus(order: Order, status: OrderStatus): F[OrderStatusResponse] = waitForOrderStatus(order.assetPair, order.id(), status)
  def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: OrderStatus): F[OrderStatusResponse]

  def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[exchange.ExchangeTransaction]]
  def waitForTransactionsByOrder(id: Order.Id)(pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]]

  def waitForCurrentOffset(pred: Long => Boolean): F[Long]
}

object DexApi {
  implicit val functorK: FunctorK[DexApi] = Derive.functorK[DexApi]
  implicit val balanceReads = Reads.map[Long].map { xs =>
    xs.map { case (k, v) => AssetPair.extractAssetId(k).get -> v }
  }

  // TODO
  implicit val exchangeTxReads: Reads[exchange.ExchangeTransaction] = Reads { json =>
    JsSuccess(TransactionFactory.fromSignedRequest(json).right.get.asInstanceOf[exchange.ExchangeTransaction])
  }

  implicit val orderWrites: Writes[Order] = Writes(_.json())

  implicit class AssetPairExt(val p: AssetPair) extends AnyVal {
    def toUri: String = s"${AssetPair.assetIdStr(p.amountAsset)}/${AssetPair.assetIdStr(p.priceAsset)}"
  }

  private def cancelRequest(sender: KeyPair, orderId: String): CancelOrderRequest = {
    val req       = CancelOrderRequest(sender, Some(ByteStr.decodeBase58(orderId).get), None, Array.emptyByteArray)
    val signature = crypto.sign(sender, req.toSign)
    req.copy(signature = signature)
  }

  private def batchCancelRequest(sender: KeyPair, timestamp: Long): CancelOrderRequest = {
    val req       = CancelOrderRequest(sender, None, Some(timestamp), Array.emptyByteArray)
    val signature = crypto.sign(sender, req.toSign)
    req.copy(signature = signature)
  }

  def apply[F[_]](apiKey: String,
                  host: => InetSocketAddress)(implicit M: ThrowableMonadError[F], W: CanWait[F], httpBackend: SttpBackend[F, Nothing]): DexApi[F] =
    new DexApi[F] {
      private val ops = FOps[F]
      import ops._

      def apiUri = s"http://${host.getAddress.getHostAddress}:${host.getPort}/matcher"

      override def publicKey: F[PublicKey] = {
        val req = sttp
          .get(uri"$apiUri")
          .response(asJson[ByteStr])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse).map(x => PublicKey(x))
      }

      override def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Map[Asset, Long]] = {
        val req = sttp
          .get(uri"$apiUri/balance/reserved/${Base58.encode(of.publicKey)}")
          .headers(timestampAndSignatureHeaders(of, timestamp))
          .response(asJson[Map[Asset, Long]])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse)
      }

      override def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Map[Asset, Long]] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/tradableBalance/${of.publicKey.toAddress.stringRepr}")
          .headers(timestampAndSignatureHeaders(of, timestamp))
          .response(asJson[Map[Asset, Long]])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse)
      }

      override def tryPlace(order: Order): F[Either[MatcherError, Unit]] = try_ {
        sttp
          .post(uri"$apiUri/orderbook")
          .followRedirects(false)
          .body(order)
          .mapResponse(_ => ())
          .tag("requestId", UUID.randomUUID())
      }

      // replace with tryPlace
      override def place(order: Order): F[MatcherResponse] = {
        val req = sttp.post(uri"$apiUri/orderbook").body(order).response(asJson[MatcherResponse]).tag("requestId", UUID.randomUUID())
        httpBackend.send(req).flatMap(parseResponse)
      }

      override def placeMarket(order: Order): F[MatcherResponse] = {
        val req = sttp.post(uri"$apiUri/orderbook/market").body(order).response(asJson[MatcherResponse]).tag("requestId", UUID.randomUUID())
        httpBackend.send(req).flatMap(parseResponse)
      }

      override def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, MatcherStatusResponse]] =
        tryParse[MatcherStatusResponse] {
          val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
          sttp
            .post(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
            .followRedirects(false)
            .body(body)
            .contentType("application/json", "UTF-8")
            .response(asJson[MatcherStatusResponse])
            .tag("requestId", UUID.randomUUID())
        }

      // replace with tryCancel
      override def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[MatcherStatusResponse] = {
        val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
        val req =
          sttp
            .post(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
            .body(body)
            .contentType("application/json", "UTF-8")
            .response(asJson[MatcherStatusResponse])
            .tag("requestId", UUID.randomUUID())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Unit] = {
        val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
        val req =
          sttp
            .post(uri"$apiUri/orderbook/cancel")
            .body(body)
            .contentType("application/json", "UTF-8")
            .mapResponse(_ => ())
            .tag("requestId", UUID.randomUUID())
        repeatUntil(httpBackend.send(req), 1.second)(_.isSuccess).map(_ => ())
      }

      override def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Unit] = {
        val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
        val req =
          sttp
            .post(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
            .body(body)
            .contentType("application/json", "UTF-8")
            .mapResponse(_ => ())
            .tag("requestId", UUID.randomUUID())
        repeatUntil(httpBackend.send(req), 1.second)(_.isSuccess).map(_ => ())
      }

      override def cancelWithApiKey(id: Order.Id): F[MatcherStatusResponse] = {
        val req = sttp
          .post(uri"$apiUri/orders/cancel/${id.toString}")
          .headers(apiKeyHeaders)
          .contentType("application/json", "UTF-8")
          .response(asJson[MatcherStatusResponse])
          .tag("requestId", UUID.randomUUID())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def tryOrderStatus(assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, OrderStatusResponse]] = tryParse {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/$id")
          .followRedirects(false)
          .response(asJson[OrderStatusResponse])
          .tag("requestId", UUID.randomUUID())
      }

      override def orderStatus(assetPair: AssetPair, id: Order.Id): F[OrderStatusResponse] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/$id")
          .response(asJson[OrderStatusResponse])
          .tag("requestId", UUID.randomUUID())
        repeatUntilResponse[OrderStatusResponse](httpBackend.send(req), 1.second)(_.code == StatusCodes.Ok)
      }

      override def transactionsByOrder(id: Order.Id): F[List[exchange.ExchangeTransaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/$id").response(asJson[List[exchange.ExchangeTransaction]]).tag("requestId", UUID.randomUUID())
        repeatUntilResponse[List[exchange.ExchangeTransaction]](httpBackend.send(req), 1.second)(_.code == StatusCodes.Ok)
      }

      override def orderHistory(owner: KeyPair,
                                activeOnly: Option[Boolean] = None,
                                timestamp: Long = System.currentTimeMillis()): F[List[OrderBookHistoryItem]] = {
        // todo: Address ?
        val req = sttp
          .get(appendActiveOnly(uri"$apiUri/orderbook/${Base58.encode(owner.publicKey)}", activeOnly))
          .headers(timestampAndSignatureHeaders(owner, timestamp))
          .response(asJson[List[OrderBookHistoryItem]])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def orderHistoryWithApiKey(owner: com.wavesplatform.account.Address,
                                          activeOnly: Option[Boolean] = None): F[List[OrderBookHistoryItem]] = {
        val req = sttp
          .get(appendActiveOnly(uri"$apiUri/orders/${owner.stringRepr}", activeOnly))
          .headers(apiKeyHeaders)
          .response(asJson[List[OrderBookHistoryItem]])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def orderHistoryByPair(owner: KeyPair,
                                      assetPair: AssetPair,
                                      activeOnly: Option[Boolean] = None,
                                      timestamp: Long = System.currentTimeMillis()): F[List[OrderBookHistoryItem]] = {
        val req = sttp
          .get(
            appendActiveOnly(
              uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/publicKey/${Base58.encode(owner.publicKey)}",
              activeOnly
            ))
          .headers(timestampAndSignatureHeaders(owner, timestamp))
          .response(asJson[List[OrderBookHistoryItem]])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def allOrderBooks: F[MarketDataInfo] = {
        val req = sttp
          .get(uri"$apiUri/orderbook")
          .response(asJson[MarketDataInfo])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse)
      }

      override def tryOrderBook(assetPair: AssetPair): F[Either[MatcherError, OrderBookResponse]] = tryParse {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .followRedirects(false)
          .response(asJson[OrderBookResponse])
          .tag("requestId", UUID.randomUUID())
      }

      override def orderBook(assetPair: AssetPair): F[OrderBookResponse] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .response(asJson[OrderBookResponse])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def tryOrderBookStatus(assetPair: AssetPair): F[Either[MatcherError, MarketStatusResponse]] = tryParse {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/status")
          .followRedirects(false)
          .headers(apiKeyHeaders)
          .response(asJson[MarketStatusResponse])
          .tag("requestId", UUID.randomUUID())
      }

      override def orderBookStatus(assetPair: AssetPair): F[MarketStatusResponse] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/status")
          .headers(apiKeyHeaders)
          .response(asJson[MarketStatusResponse])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse[MarketStatusResponse])
      }

      override def tryDeleteOrderBook(assetPair: AssetPair): F[Either[MatcherError, Unit]] = try_ {
        sttp
          .delete(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .followRedirects(false)
          .headers(apiKeyHeaders)
          .mapResponse(_ => ())
          .tag("requestId", UUID.randomUUID())
      }

      override def tryUpsertRate(asset: Asset, rate: Double): F[Either[MatcherError, (StatusCode, RatesResponse)]] = {
        val req =
          sttp
            .put(uri"$apiUri/settings/rates/${AssetPair.assetIdStr(asset)}")
            .body(Json.stringify(Json.toJson(rate)))
            .contentType("application/json", "UTF-8")
            .headers(apiKeyHeaders)
            .response(asJson[RatesResponse])
            .tag("requestId", UUID.randomUUID())

        for {
          rawResp <- httpBackend.send(req)
          resp    <- parseTryResponseEither[MatcherError, RatesResponse](rawResp)
        } yield resp.map(rawResp.code -> _)
      }

      override def upsertRate(asset: Asset, rate: Double): F[(StatusCode, RatesResponse)] = {
        val req =
          sttp
            .put(uri"$apiUri/settings/rates/${AssetPair.assetIdStr(asset)}")
            .body(Json.stringify(Json.toJson(rate)))
            .contentType("application/json", "UTF-8")
            .headers(apiKeyHeaders)
            .response(asJson[RatesResponse])
            .tag("requestId", UUID.randomUUID())

        for {
          rawResp <- httpBackend.send(req)
          resp    <- parseResponse[RatesResponse](rawResp)
        } yield (rawResp.code, resp)
      }

      override def tryDeleteRate(asset: Asset): F[Either[MatcherError, RatesResponse]] = tryParse {
        sttp
          .delete(uri"$apiUri/settings/rates/${AssetPair.assetIdStr(asset)}")
          .contentType("application/json", "UTF-8")
          .headers(apiKeyHeaders)
          .response(asJson[RatesResponse])
          .tag("requestId", UUID.randomUUID())
      }

      override def deleteRate(asset: Asset): F[RatesResponse] = {
        val req =
          sttp
            .delete(uri"$apiUri/settings/rates/${AssetPair.assetIdStr(asset)}")
            .contentType("application/json", "UTF-8")
            .headers(apiKeyHeaders)
            .response(asJson[RatesResponse])
            .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse)
      }

      override def rates: F[Map[Asset, Double]] = {
        val req = sttp
          .get(uri"$apiUri/settings/rates")
          .headers(apiKeyHeaders)
          .response(asJson[Map[Asset, Double]])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse)
      }

      override def currentOffset: F[Long] = {
        val req = sttp
          .get(uri"$apiUri/debug/currentOffset")
          .headers(apiKeyHeaders)
          .response(asString("UTF-8"))
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_) => M.raiseError[Long](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(raw) =>
              val r = Longs.tryParse(raw)
              if (r == null) M.raiseError[Long](new RuntimeException(s"Can't parse the response as Long: $raw"))
              else M.pure(r)
          }
        }
      }

      override def lastOffset: F[Long] = {
        val req = sttp
          .get(uri"$apiUri/debug/lastOffset")
          .headers(apiKeyHeaders)
          .response(asString("UTF-8"))
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(_) => M.raiseError[Long](new RuntimeException(s"The server returned an error: ${resp.code}"))
            case Right(raw) =>
              val r = Longs.tryParse(raw)
              if (r == null) M.raiseError[Long](new RuntimeException(s"Can't parse the response as Long: $raw"))
              else M.pure(r)
          }
        }
      }

      override def allSnapshotOffsets: F[Map[String, Long]] = {
        val req = sttp
          .get(uri"$apiUri/debug/allSnapshotOffsets")
          .headers(apiKeyHeaders)
          .response(asJson[Map[String, Long]])
          .tag("requestId", UUID.randomUUID())

        httpBackend.send(req).flatMap(parseResponse[Map[String, Long]])
      }

      override def waitReady: F[Unit] = {
        val req = sttp.get(uri"$apiUri").mapResponse(_ => ())

        def loop(): F[Response[Unit]] = M.handleErrorWith(httpBackend.send(req)) {
          case _: SocketException => W.wait(1.second).flatMap(_ => loop())
          case NonFatal(e)        => M.raiseError(e)
        }

        repeatUntil(loop(), 1.second)(_.code == StatusCodes.Ok).map(_ => ())
      }

      override def config: F[Config] = ???

      override def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: OrderStatusResponse => Boolean): F[OrderStatusResponse] =
        repeatUntil(orderStatus(assetPair, id), 1.second)(pred)

      override def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: OrderStatus): F[OrderStatusResponse] =
        waitForOrder(assetPair, id)(_.status == status)

      override def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[exchange.ExchangeTransaction]] =
        waitForTransactionsByOrder(id)(_.lengthCompare(atLeast) >= 0)

      override def waitForTransactionsByOrder(id: Order.Id)(
          pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]] =
        repeatUntil[List[exchange.ExchangeTransaction]](transactionsByOrder(id), 1.second)(pred)

      override def waitForCurrentOffset(pred: Long => Boolean): F[Long] = repeatUntil[Long](currentOffset, 1.second)(pred)

      private def tryParse[T](req: RequestT[Id, Either[DeserializationError[JsError], T], Nothing]): F[Either[MatcherError, T]] =
        httpBackend.send(req).flatMap(parseTryResponseEither[MatcherError, T])

      private def try_(req: RequestT[Id, Unit, Nothing]): F[Either[MatcherError, Unit]] =
        httpBackend.send(req).flatMap(parseTryResponse[MatcherError, Unit])

      private def appendActiveOnly(uri: Uri, activeOnly: Option[Boolean]): Uri =
        activeOnly.fold(uri)(x => uri.copy(queryFragments = List(QueryFragment.KeyValue("activeOnly", x.toString))))

      private def timestampAndSignatureHeaders(owner: KeyPair, timestamp: Long): Map[String, String] = Map(
        "Timestamp" -> timestamp.toString,
        "Signature" -> Base58.encode(crypto.sign(owner, owner.publicKey ++ Longs.toByteArray(timestamp)))
      )

      private def apiKeyHeaders: Map[String, String] = Map("X-API-Key" -> apiKey)
    }
}
