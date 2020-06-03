package com.wavesplatform.dex.it.dex

import java.net.{InetSocketAddress, SocketException}
import java.util.UUID

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.tagless.{Derive, FunctorK}
import com.google.common.primitives.Longs
import com.softwaremill.sttp.Uri.QueryFragment
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{SttpBackend, MonadError => _, _}
import com.wavesplatform.dex.api.{MatcherResponse => _, _}
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderJson.orderFormat
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.dex.it.api.responses.dex._
import com.wavesplatform.dex.it.fp.{CanWait, FOps, RepeatRequestOptions, ThrowableMonadError}
import com.wavesplatform.dex.it.json._
import com.wavesplatform.dex.it.sttp.ResponseParsers.asLong
import com.wavesplatform.dex.it.sttp.SttpBackendOps
import com.wavesplatform.wavesj.transactions.ExchangeTransaction
import play.api.libs.json.{JsResultException, Json}

import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

trait DexApi[F[_]] extends HasWaitReady[F] {
  // Won't work with type TryF[T] = F[Either[MatcherError, T]]

  def tryPublicKey: F[Either[MatcherError, ApiMatcherPublicKey]]

  def tryReservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, ApiBalance]]
  def tryReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, ApiBalance]]

  def tryTradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, ApiBalance]]

  def tryPlace(order: Order): F[Either[MatcherError, ApiSuccessfulPlace]]
  def tryPlaceMarket(order: Order): F[Either[MatcherError, ApiSuccessfulPlace]]

  def tryCancel(owner: KeyPair, order: Order): F[Either[MatcherError, MatcherStatusResponse]] = tryCancel(owner, order.assetPair, order.id())
  def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, MatcherStatusResponse]]
  def tryCancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, MatcherStatusResponse]]

  // TODO Response type in DEX-548
  def tryCancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Unit]]
  def tryCancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Unit]]
  def tryCancelAllByIdsWithApiKey(owner: Address, orderIds: Set[Order.Id], xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, Unit]]

  def tryOrderStatus(order: Order): F[Either[MatcherError, OrderStatusResponse]] = tryOrderStatus(order.assetPair, order.id())
  def tryOrderStatus(assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, OrderStatusResponse]]

  def tryOrderStatusInfoByIdWithApiKey(owner: Address,
                                       orderId: Order.Id,
                                       xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, ApiOrderBookHistoryItem]]

  def tryOrderStatusInfoByIdWithSignature(owner: KeyPair,
                                          orderId: Order.Id,
                                          timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, ApiOrderBookHistoryItem]]

  def tryTransactionsByOrder(id: Order.Id): F[Either[MatcherError, List[ExchangeTransaction]]]

  /**
    * param @activeOnly Server treats this parameter as false if it wasn't specified
    */
  def tryOrderHistory(owner: KeyPair,
                      activeOnly: Option[Boolean] = None,
                      closedOnly: Option[Boolean] = None,
                      timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[ApiOrderBookHistoryItem]]]

  /**
    * param @activeOnly Server treats this parameter as true if it wasn't specified
    */
  def tryOrderHistoryWithApiKey(owner: Address,
                                activeOnly: Option[Boolean] = None,
                                closedOnly: Option[Boolean] = None,
                                xUserPublicKey: Option[PublicKey] = None): F[Either[MatcherError, List[ApiOrderBookHistoryItem]]]

  /**
    * param @activeOnly Server treats this parameter as false if it wasn't specified
    */
  def tryOrderHistoryByPair(owner: KeyPair,
                            assetPair: AssetPair,
                            activeOnly: Option[Boolean] = None,
                            closedOnly: Option[Boolean] = None,
                            timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[ApiOrderBookHistoryItem]]]

  def tryAllOrderBooks: F[Either[MatcherError, ApiTradingMarkets]]

  def tryOrderBook(assetPair: AssetPair): F[Either[MatcherError, ApiV0OrderBook]]
  def tryOrderBook(assetPair: AssetPair, depth: Int): F[Either[MatcherError, ApiV0OrderBook]]

  def tryOrderBookInfo(assetPair: AssetPair): F[Either[MatcherError, ApiOrderBookInfo]]
  def tryOrderBookStatus(assetPair: AssetPair): F[Either[MatcherError, ApiMarketStatus]]

  def tryDeleteOrderBook(assetPair: AssetPair): F[Either[MatcherError, Unit]] // TODO

  def tryUpsertRate(asset: Asset, rate: Double): F[Either[MatcherError, (StatusCode, ApiMessage)]]
  def tryDeleteRate(asset: Asset): F[Either[MatcherError, ApiMessage]]
  def tryRates: F[Either[MatcherError, ApiRates]]

  def tryCurrentOffset: F[Either[MatcherError, ApiOffset]]
  def tryLastOffset: F[Either[MatcherError, ApiOffset]]
  def tryOldestSnapshotOffset: F[Either[MatcherError, ApiOffset]]
  def tryAllSnapshotOffsets: F[Either[MatcherError, ApiSnapshotOffsets]]
  def trySaveSnapshots: F[Either[MatcherError, Unit]]

  def trySettings: F[Either[MatcherError, ApiMatcherPublicSettings]]

  // TODO move

  def waitForOrder(order: Order)(pred: OrderStatusResponse => Boolean): F[OrderStatusResponse] = waitForOrder(order.assetPair, order.id())(pred)
  def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: OrderStatusResponse => Boolean): F[OrderStatusResponse]

  def waitForOrderPlacement(order: Order): F[ApiSuccessfulPlace]

  def waitForOrderHistory[A](owner: KeyPair, activeOnly: Option[Boolean])(
      pred: List[ApiOrderBookHistoryItem] => Boolean): F[List[ApiOrderBookHistoryItem]]

  def waitForOrderStatus(order: Order, status: OrderStatus): F[OrderStatusResponse] = waitForOrderStatus(order.assetPair, order.id(), status)
  def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: OrderStatus): F[OrderStatusResponse]

  def waitForTransactionsByOrder(order: Order, atLeast: Int): F[List[ExchangeTransaction]] = waitForTransactionsByOrder(order.id(), atLeast)
  def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[ExchangeTransaction]]

  def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]]

  def waitForCurrentOffset(pred: Long => Boolean): F[ApiOffset]
}

object DexApi {

  implicit val functorK: FunctorK[DexApi] = Derive.functorK[DexApi]

  implicit class AssetPairExt(val p: AssetPair) extends AnyVal {
    def toUri: String = s"${p.amountAsset.toString}/${p.priceAsset.toString}"
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

  // noinspection ScalaStyle
  def apply[F[_]](apiKey: String,
                  host: => InetSocketAddress)(implicit M: ThrowableMonadError[F], W: CanWait[F], httpBackend: SttpBackend[F, Nothing]): DexApi[F] =
    new DexApi[F] {

      private val ops     = FOps[F]; import ops._
      private val sttpOps = SttpBackendOps[F, MatcherError]; import sttpOps._

      def apiUri: String = {
        val savedHost = host
        s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}/matcher"
      }

      override def tryPublicKey: F[Either[MatcherError, ApiMatcherPublicKey]] = tryParseJson(sttp.get(uri"$apiUri"))

      override def tryReservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Map[Asset, Long]]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/balance/reserved/${Base58.encode(of.publicKey)}")
            .headers(timestampAndSignatureHeaders(of, timestamp))
        }

      override def tryReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, Map[Asset, Long]]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/balance/reserved/${Base58.encode(of.publicKey)}")
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
        }

      override def tryTradableBalance(of: KeyPair,
                                      assetPair: AssetPair,
                                      timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Map[Asset, Long]]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/tradableBalance/${of.publicKey.toAddress.stringRepr}")
            .headers(timestampAndSignatureHeaders(of, timestamp))
        }

      override def tryPlace(order: Order): F[Either[MatcherError, ApiSuccessfulPlace]] = tryParseJson {
        sttp
          .post(uri"$apiUri/orderbook")
          .readTimeout(3.minutes) // TODO find way to decrease timeout!
          .followRedirects(false) // TODO move ?
          .body(order)
      }

      override def tryPlaceMarket(order: Order): F[Either[MatcherError, ApiSuccessfulPlace]] =
        tryParseJson(sttp.post(uri"$apiUri/orderbook/market").body(order))

      override def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, MatcherStatusResponse]] =
        tryParseJson {
          val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
          sttp
            .post(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
            .readTimeout(3.minutes) // TODO find way to decrease timeout!
            .followRedirects(false)
            .body(body)
            .contentType("application/json", "UTF-8")
        }

      override def tryCancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Unit]] = tryUnit {
        val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
        sttp
          .post(uri"$apiUri/orderbook/cancel")
          .body(body)
          .contentType("application/json", "UTF-8")
      }

      override def tryCancelAllByPair(owner: KeyPair,
                                      assetPair: AssetPair,
                                      timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Unit]] = tryUnit {
        val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
        sttp
          .post(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
          .body(body)
          .contentType("application/json", "UTF-8")
      }

      override def tryCancelAllByIdsWithApiKey(owner: Address,
                                               orderIds: Set[Order.Id],
                                               xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, Unit]] = tryUnit {
        sttp
          .post(uri"$apiUri/orders/$owner/cancel")
          .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
          .body(Json.stringify(Json.toJson(orderIds)))
          .contentType("application/json", "UTF-8")
      }

      override def tryCancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, MatcherStatusResponse]] =
        tryParseJson {
          sttp
            .post(uri"$apiUri/orders/cancel/${id.toString}")
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
            .contentType("application/json", "UTF-8")
        }

      override def tryOrderStatus(assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, OrderStatusResponse]] = {
        tryParseJson {
          sttp
            .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/$id")
            .readTimeout(3.minutes) // TODO find way to decrease timeout!
            .followRedirects(false)
        }
      }

      override def tryOrderStatusInfoByIdWithApiKey(owner: Address,
                                                    orderId: Order.Id,
                                                    xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, ApiOrderBookHistoryItem]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/orders/$owner/${orderId.toString}")
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
        }

      override def tryOrderStatusInfoByIdWithSignature(owner: KeyPair,
                                                       orderId: Order.Id,
                                                       timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, ApiOrderBookHistoryItem]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/orders/${owner.publicKey}/${orderId.toString}")
            .headers(timestampAndSignatureHeaders(owner, timestamp))
        }

      override def tryTransactionsByOrder(id: Order.Id): F[Either[MatcherError, List[ExchangeTransaction]]] =
        tryParseJson(sttp.get(uri"$apiUri/transactions/$id"))

      override def tryOrderHistory(owner: KeyPair,
                                   activeOnly: Option[Boolean] = None,
                                   closedOnly: Option[Boolean] = None,
                                   timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[ApiOrderBookHistoryItem]]] =
        tryParseJson {
          sttp
            .get(appendFilters(uri"$apiUri/orderbook/${Base58.encode(owner.publicKey)}", activeOnly, closedOnly))
            .headers(timestampAndSignatureHeaders(owner, timestamp))
        }

      override def tryOrderHistoryWithApiKey(owner: Address,
                                             activeOnly: Option[Boolean] = None,
                                             closedOnly: Option[Boolean] = None,
                                             xUserPublicKey: Option[PublicKey] = None): F[Either[MatcherError, List[ApiOrderBookHistoryItem]]] =
        tryParseJson {
          sttp
            .get(appendFilters(uri"$apiUri/orders/${owner.stringRepr}", activeOnly, closedOnly))
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
        }

      override def tryOrderHistoryByPair(owner: KeyPair,
                                         assetPair: AssetPair,
                                         activeOnly: Option[Boolean] = None,
                                         closedOnly: Option[Boolean] = None,
                                         timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[ApiOrderBookHistoryItem]]] =
        tryParseJson {
          sttp
            .get(
              appendFilters(
                uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/publicKey/${Base58.encode(owner.publicKey)}",
                activeOnly,
                closedOnly
              )
            )
            .headers(timestampAndSignatureHeaders(owner, timestamp))
        }

      override def tryAllOrderBooks: F[Either[MatcherError, ApiTradingMarkets]] = tryParseJson(sttp.get(uri"$apiUri/orderbook"))

      override def tryOrderBook(assetPair: AssetPair): F[Either[MatcherError, ApiV0OrderBook]] = tryParseJson {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .followRedirects(false)
      }

      override def tryOrderBook(assetPair: AssetPair, depth: Int): F[Either[MatcherError, ApiV0OrderBook]] = tryParseJson {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}?depth=$depth")
          .followRedirects(true)
      }

      override def tryOrderBookInfo(assetPair: AssetPair): F[Either[MatcherError, ApiOrderBookInfo]] = tryParseJson {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/info")
          .followRedirects(false)
      }

      override def tryOrderBookStatus(assetPair: AssetPair): F[Either[MatcherError, ApiMarketStatus]] = tryParseJson {
        sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/status")
          .followRedirects(false)
          .headers(apiKeyHeaders)
      }

      override def tryDeleteOrderBook(assetPair: AssetPair): F[Either[MatcherError, Unit]] = tryUnit {
        sttp
          .delete(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .followRedirects(false)
          .headers(apiKeyHeaders)
      }

      // TODO
      override def tryUpsertRate(asset: Asset, rate: Double): F[Either[MatcherError, (StatusCode, ApiMessage)]] = {
        val req =
          sttp
            .put(uri"$apiUri/settings/rates/${asset.toString}")
            .body(Json.stringify(Json.toJson(rate)))
            .contentType("application/json", "UTF-8")
            .headers(apiKeyHeaders)
            .response(asJson[ApiMessage])
            .tag("requestId", UUID.randomUUID)

        for {
          rawResp <- httpBackend.send(req)
          resp    <- parseTryResponseEither[MatcherError, ApiMessage](rawResp)
        } yield resp.map(rawResp.code -> _)
      }

      override def tryDeleteRate(asset: Asset): F[Either[MatcherError, ApiMessage]] = tryParseJson {
        sttp
          .delete(uri"$apiUri/settings/rates/${asset.toString}")
          .contentType("application/json", "UTF-8")
          .headers(apiKeyHeaders)
      }

      override def tryRates: F[Either[MatcherError, ApiRates]] =
        tryParseJson[ApiRates](sttp.get(uri"$apiUri/settings/rates").headers(apiKeyHeaders))

      override def tryCurrentOffset: F[Either[MatcherError, ApiOffset]] = tryParse {
        sttp
          .get(uri"$apiUri/debug/currentOffset")
          .headers(apiKeyHeaders)
          .response(asLong)
      }

      override def tryLastOffset: F[Either[MatcherError, ApiOffset]] = tryParse {
        sttp
          .get(uri"$apiUri/debug/lastOffset")
          .headers(apiKeyHeaders)
          .response(asLong)
      }

      override def tryOldestSnapshotOffset: F[Either[MatcherError, ApiOffset]] = tryParse {
        sttp
          .get(uri"$apiUri/debug/oldestSnapshotOffset")
          .headers(apiKeyHeaders)
          .response(asLong)
      }

      override def tryAllSnapshotOffsets: F[Either[MatcherError, ApiSnapshotOffsets]] =
        tryParseJson(sttp.get(uri"$apiUri/debug/allSnapshotOffsets").headers(apiKeyHeaders))

      override def trySaveSnapshots: F[Either[MatcherError, Unit]] = tryUnit {
        sttp.post(uri"$apiUri/debug/saveSnapshots").headers(apiKeyHeaders)
      }

      override def waitReady: F[Unit] = {
        def request: F[Boolean] = M.handleErrorWith(tryAllOrderBooks.map(_.isRight)) {
          case _: SocketException | _: JsResultException => M.pure(false)
          case NonFatal(e)                               => M.raiseError(e)
        }

        // Sometimes container start during 20 seconds! https://github.com/docker/for-mac/issues/1183
        repeatUntil(request, RepeatRequestOptions(1.second, 60 + 20))(_ == true).map(_ => ())
      }

      override def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: OrderStatusResponse => Boolean): F[OrderStatusResponse] =
        repeatUntil(tryOrderStatus(assetPair, id), RepeatRequestOptions(1.second, 60)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForOrderPlacement(order: Order): F[ApiSuccessfulPlace] = repeatUntil(tryPlace(order))(_.isRight).map(_.explicitGet())

      def waitForOrderHistory[A](owner: KeyPair, activeOnly: Option[Boolean])(
          pred: List[ApiOrderBookHistoryItem] => Boolean): F[List[ApiOrderBookHistoryItem]] =
        repeatUntil[Either[MatcherError, List[ApiOrderBookHistoryItem]]](tryOrderHistory(owner, activeOnly), RepeatRequestOptions(1.second, 60)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: OrderStatus): F[OrderStatusResponse] =
        waitForOrder(assetPair, id)(_.status == status)

      override def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[ExchangeTransaction]] =
        waitForTransactionsByOrder(id)(_.lengthCompare(atLeast) >= 0)

      override def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]] =
        repeatUntil[Either[MatcherError, List[ExchangeTransaction]]](tryTransactionsByOrder(id), RepeatRequestOptions(1.second, 60)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForCurrentOffset(pred: Long => Boolean): F[ApiOffset] =
        repeatUntil[Either[MatcherError, ApiOffset]](tryCurrentOffset, RepeatRequestOptions(1.second, 120)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def trySettings: F[Either[MatcherError, ApiMatcherPublicSettings]] = tryParseJson {
        sttp
          .get(uri"$apiUri/settings")
          .headers(apiKeyHeaders)
      }

      private def appendFilters(uri: Uri, activeOnly: Option[Boolean], closedOnly: Option[Boolean]): Uri = {
        val activeOnlyQuery = boolQueryFragments("activeOnly", activeOnly)
        val closedOnlyQuery = boolQueryFragments("closedOnly", closedOnly)
        uri.copy(queryFragments = activeOnlyQuery ++ closedOnlyQuery)
      }

      private def boolQueryFragments(name: String, x: Option[Boolean]): List[QueryFragment] =
        x.fold(List.empty[QueryFragment])(x => List(QueryFragment.KeyValue(name, x.toString)))

      private def timestampAndSignatureHeaders(owner: KeyPair, timestamp: Long): Map[String, String] = Map(
        "Timestamp" -> timestamp.toString,
        "Signature" -> Base58.encode(crypto.sign(owner, owner.publicKey ++ Longs.toByteArray(timestamp)))
      )

      private val apiKeyHeaders: Map[String, String]                      = Map("X-API-Key"         -> apiKey)
      private def userPublicKeyHeaders(x: PublicKey): Map[String, String] = Map("X-User-Public-Key" -> x.base58)

      private def apiKeyWithUserPublicKeyHeaders(xUserPublicKey: Option[PublicKey]): Map[String, String] = {
        apiKeyHeaders ++ xUserPublicKey.fold(Map.empty[String, String])(userPublicKeyHeaders)
      }
    }
}
