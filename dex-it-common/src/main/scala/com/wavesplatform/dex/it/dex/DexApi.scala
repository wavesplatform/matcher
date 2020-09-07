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
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
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

  def tryPublicKey: F[Either[MatcherError, HttpMatcherPublicKey]]
  def tryPublicKeyWithResponse: F[(Response[_], Either[MatcherError, HttpMatcherPublicKey])]

  def tryReservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpBalance]]
  def tryReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpBalance]]

  def tryTradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpBalance]]

  def tryPlace(order: Order): F[Either[MatcherError, HttpSuccessfulPlace]]
  def tryPlaceMarket(order: Order): F[Either[MatcherError, HttpSuccessfulPlace]]

  def tryCancel(owner: KeyPair, order: Order): F[Either[MatcherError, HttpSuccessfulSingleCancel]] = tryCancel(owner, order.assetPair, order.id())
  def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, HttpSuccessfulSingleCancel]]
  def tryCancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpSuccessfulSingleCancel]]

  def tryCancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpSuccessfulBatchCancel]]

  def tryCancelAllByPair(owner: KeyPair,
                         assetPair: AssetPair,
                         timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpSuccessfulBatchCancel]]

  def tryCancelAllByIdsWithApiKey(owner: Address,
                                  orderIds: Set[Order.Id],
                                  xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpSuccessfulBatchCancel]]

  def tryOrderStatus(order: Order): F[Either[MatcherError, HttpOrderStatus]] = tryOrderStatus(order.assetPair, order.id())
  def tryOrderStatus(assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, HttpOrderStatus]]

  def tryOrderStatusInfoByIdWithApiKey(owner: Address,
                                       orderId: Order.Id,
                                       xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpOrderBookHistoryItem]]

  def tryOrderStatusInfoByIdWithSignature(owner: KeyPair,
                                          orderId: Order.Id,
                                          timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpOrderBookHistoryItem]]

  def tryTransactionsByOrder(id: Order.Id): F[Either[MatcherError, List[ExchangeTransaction]]]

  /**
    * param @activeOnly Server treats this parameter as false if it wasn't specified
    */
  def tryOrderHistory(owner: KeyPair,
                      activeOnly: Option[Boolean] = None,
                      closedOnly: Option[Boolean] = None,
                      timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[HttpOrderBookHistoryItem]]]

  /**
    * param @activeOnly Server treats this parameter as true if it wasn't specified
    */
  def tryOrderHistoryWithApiKey(owner: Address,
                                activeOnly: Option[Boolean] = None,
                                closedOnly: Option[Boolean] = None,
                                xUserPublicKey: Option[PublicKey] = None): F[Either[MatcherError, List[HttpOrderBookHistoryItem]]]

  /**
    * param @activeOnly Server treats this parameter as false if it wasn't specified
    */
  def tryOrderHistoryByPair(owner: KeyPair,
                            assetPair: AssetPair,
                            activeOnly: Option[Boolean] = None,
                            closedOnly: Option[Boolean] = None,
                            timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[HttpOrderBookHistoryItem]]]

  def tryAllOrderBooks: F[Either[MatcherError, HttpTradingMarkets]]

  def tryOrderBook(assetPair: AssetPair): F[Either[MatcherError, HttpV0OrderBook]]
  def tryOrderBook(assetPair: AssetPair, depth: Int): F[Either[MatcherError, HttpV0OrderBook]]

  def tryOrderBookInfo(assetPair: AssetPair): F[Either[MatcherError, HttpOrderBookInfo]]
  def tryOrderBookStatus(assetPair: AssetPair): F[Either[MatcherError, HttpMarketStatus]]

  def tryDeleteOrderBook(assetPair: AssetPair): F[Either[MatcherError, HttpMessage]]

  def tryUpsertRate(asset: Asset, rate: Double): F[Either[MatcherError, (StatusCode, HttpMessage)]]
  def tryDeleteRate(asset: Asset): F[Either[MatcherError, HttpMessage]]
  def tryRates: F[Either[MatcherError, HttpRates]]

  def tryCurrentOffset: F[Either[MatcherError, HttpOffset]]
  def tryLastOffset: F[Either[MatcherError, HttpOffset]]
  def tryOldestSnapshotOffset: F[Either[MatcherError, HttpOffset]]
  def tryAllSnapshotOffsets: F[Either[MatcherError, HttpSnapshotOffsets]]
  def trySaveSnapshots: F[Either[MatcherError, Unit]]

  def trySettings: F[Either[MatcherError, HttpMatcherPublicSettings]]

  def tryWsConnections: F[Either[MatcherError, HttpWebSocketConnections]]
  def tryCloseWsConnections(oldestNumber: Int): F[Either[MatcherError, HttpMessage]]

  // TODO move

  def waitForOrder(order: Order)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus] = waitForOrder(order.assetPair, order.id())(pred)
  def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus]

  def waitForOrderPlacement(order: Order): F[HttpSuccessfulPlace]

  def waitForOrderHistory[A](owner: KeyPair, activeOnly: Option[Boolean])(
      pred: List[HttpOrderBookHistoryItem] => Boolean): F[List[HttpOrderBookHistoryItem]]

  def waitForOrderStatus(order: Order, status: HttpOrderStatus.Status): F[HttpOrderStatus] = waitForOrderStatus(order.assetPair, order.id(), status)
  def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: HttpOrderStatus.Status): F[HttpOrderStatus]

  def waitForTransactionsByOrder(order: Order, atLeast: Int): F[List[ExchangeTransaction]] = waitForTransactionsByOrder(order.id(), atLeast)
  def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[ExchangeTransaction]]

  def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]]

  def waitForCurrentOffset(pred: Long => Boolean): F[HttpOffset]

  def waitForWsConnections(pred: HttpWebSocketConnections => Boolean): F[HttpWebSocketConnections]
}

object DexApi {

  implicit val functorK: FunctorK[DexApi] = Derive.functorK[DexApi]

  implicit class AssetPairExt(val p: AssetPair) extends AnyVal {
    def toUri: String = s"${p.amountAsset.toString}/${p.priceAsset.toString}"
  }

  private def cancelRequest(sender: KeyPair, orderId: String): HttpCancelOrder = {
    val req       = HttpCancelOrder(sender, Some(ByteStr.decodeBase58(orderId).get), None, Array.emptyByteArray)
    val signature = crypto.sign(sender, req.toSign)
    req.copy(signature = signature)
  }

  private def batchCancelRequest(sender: KeyPair, timestamp: Long): HttpCancelOrder = {
    val req       = HttpCancelOrder(sender, None, Some(timestamp), Array.emptyByteArray)
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
        s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
      }

      override val tryPublicKeyWithResponse = tryParseJsonWithResponse(sttp.get(uri"$apiUri/matcher"))
      override val tryPublicKey = tryPublicKeyWithResponse.map(_._2)

      override def tryReservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Map[Asset, Long]]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/matcher/balance/reserved/${Base58.encode(of.publicKey)}")
            .headers(timestampAndSignatureHeaders(of, timestamp))
        }

      override def tryReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, Map[Asset, Long]]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/matcher/balance/reserved/${Base58.encode(of.publicKey)}")
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
        }

      override def tryTradableBalance(of: KeyPair,
                                      assetPair: AssetPair,
                                      timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, Map[Asset, Long]]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/tradableBalance/${of.publicKey.toAddress.stringRepr}")
            .headers(timestampAndSignatureHeaders(of, timestamp))
        }

      override def tryPlace(order: Order): F[Either[MatcherError, HttpSuccessfulPlace]] = tryParseJson {
        sttp
          .post(uri"$apiUri/matcher/orderbook")
          .readTimeout(3.minutes) // TODO find way to decrease timeout!
          .followRedirects(false) // TODO move ?
          .body(order)
      }

      override def tryPlaceMarket(order: Order): F[Either[MatcherError, HttpSuccessfulPlace]] =
        tryParseJson(sttp.post(uri"$apiUri/matcher/orderbook/market").body(order))

      override def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, HttpSuccessfulSingleCancel]] =
        tryParseJson {
          val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
          sttp
            .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
            .readTimeout(3.minutes) // TODO find way to decrease timeout!
            .followRedirects(false)
            .body(body)
            .contentType("application/json", "UTF-8")
        }

      override def tryCancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpSuccessfulBatchCancel]] =
        tryParseJson {
          val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
          sttp
            .post(uri"$apiUri/matcher/orderbook/cancel")
            .body(body)
            .contentType("application/json", "UTF-8")
        }

      override def tryCancelAllByPair(owner: KeyPair,
                                      assetPair: AssetPair,
                                      timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpSuccessfulBatchCancel]] = tryParseJson {
        val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
        sttp
          .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
          .body(body)
          .contentType("application/json", "UTF-8")
      }

      override def tryCancelAllByIdsWithApiKey(owner: Address,
                                               orderIds: Set[Order.Id],
                                               xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpSuccessfulBatchCancel]] = tryParseJson {
        sttp
          .post(uri"$apiUri/matcher/orders/$owner/cancel")
          .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
          .body(Json.stringify(Json.toJson(orderIds)))
          .contentType("application/json", "UTF-8")
      }

      override def tryCancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpSuccessfulSingleCancel]] =
        tryParseJson {
          sttp
            .post(uri"$apiUri/matcher/orders/cancel/${id.toString}")
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
            .contentType("application/json", "UTF-8")
        }

      override def tryOrderStatus(assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, HttpOrderStatus]] = {
        tryParseJson {
          sttp
            .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/$id")
            .readTimeout(3.minutes) // TODO find way to decrease timeout!
            .followRedirects(false)
        }
      }

      override def tryOrderStatusInfoByIdWithApiKey(owner: Address,
                                                    orderId: Order.Id,
                                                    xUserPublicKey: Option[PublicKey]): F[Either[MatcherError, HttpOrderBookHistoryItem]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/matcher/orders/$owner/${orderId.toString}")
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
        }

      override def tryOrderStatusInfoByIdWithSignature(owner: KeyPair,
                                                       orderId: Order.Id,
                                                       timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, HttpOrderBookHistoryItem]] =
        tryParseJson {
          sttp
            .get(uri"$apiUri/matcher/orderbook/${owner.publicKey}/${orderId.toString}")
            .headers(timestampAndSignatureHeaders(owner, timestamp))
        }

      override def tryTransactionsByOrder(id: Order.Id): F[Either[MatcherError, List[ExchangeTransaction]]] =
        tryParseJson(sttp.get(uri"$apiUri/matcher/transactions/$id"))

      override def tryOrderHistory(owner: KeyPair,
                                   activeOnly: Option[Boolean] = None,
                                   closedOnly: Option[Boolean] = None,
                                   timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[HttpOrderBookHistoryItem]]] =
        tryParseJson {
          sttp
            .get(appendFilters(uri"$apiUri/matcher/orderbook/${Base58.encode(owner.publicKey)}", activeOnly, closedOnly))
            .headers(timestampAndSignatureHeaders(owner, timestamp))
        }

      override def tryOrderHistoryWithApiKey(owner: Address,
                                             activeOnly: Option[Boolean] = None,
                                             closedOnly: Option[Boolean] = None,
                                             xUserPublicKey: Option[PublicKey] = None): F[Either[MatcherError, List[HttpOrderBookHistoryItem]]] =
        tryParseJson {
          sttp
            .get(appendFilters(uri"$apiUri/matcher/orders/${owner.stringRepr}", activeOnly, closedOnly))
            .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
        }

      override def tryOrderHistoryByPair(owner: KeyPair,
                                         assetPair: AssetPair,
                                         activeOnly: Option[Boolean] = None,
                                         closedOnly: Option[Boolean] = None,
                                         timestamp: Long = System.currentTimeMillis): F[Either[MatcherError, List[HttpOrderBookHistoryItem]]] =
        tryParseJson {
          sttp
            .get(
              appendFilters(
                uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/publicKey/${Base58.encode(owner.publicKey)}",
                activeOnly,
                closedOnly
              )
            )
            .headers(timestampAndSignatureHeaders(owner, timestamp))
        }

      override def tryAllOrderBooks: F[Either[MatcherError, HttpTradingMarkets]] = tryParseJson(sttp.get(uri"$apiUri/matcher/orderbook"))

      override def tryOrderBook(assetPair: AssetPair): F[Either[MatcherError, HttpV0OrderBook]] = tryParseJson {
        sttp
          .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .followRedirects(false)
      }

      override def tryOrderBook(assetPair: AssetPair, depth: Int): F[Either[MatcherError, HttpV0OrderBook]] = tryParseJson {
        sttp
          .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}?depth=$depth")
          .followRedirects(true)
      }

      override def tryOrderBookInfo(assetPair: AssetPair): F[Either[MatcherError, HttpOrderBookInfo]] = tryParseJson {
        sttp
          .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/info")
          .followRedirects(false)
      }

      override def tryOrderBookStatus(assetPair: AssetPair): F[Either[MatcherError, HttpMarketStatus]] = tryParseJson {
        sttp
          .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/status")
          .followRedirects(false)
          .headers(apiKeyHeaders)
      }

      override def tryDeleteOrderBook(assetPair: AssetPair): F[Either[MatcherError, HttpMessage]] = tryParseJson {
        sttp
          .delete(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .followRedirects(false)
          .headers(apiKeyHeaders)
      }

      // TODO
      override def tryUpsertRate(asset: Asset, rate: Double): F[Either[MatcherError, (StatusCode, HttpMessage)]] = {
        val req =
          sttp
            .put(uri"$apiUri/matcher/settings/rates/${asset.toString}")
            .body(Json.stringify(Json.toJson(rate)))
            .contentType("application/json", "UTF-8")
            .headers(apiKeyHeaders)
            .response(asJson[HttpMessage])
            .tag("requestId", UUID.randomUUID)

        for {
          rawResp <- httpBackend.send(req)
          resp    <- parseTryResponseEither[MatcherError, HttpMessage](rawResp)
        } yield resp.map(rawResp.code -> _)
      }

      override def tryDeleteRate(asset: Asset): F[Either[MatcherError, HttpMessage]] = tryParseJson {
        sttp
          .delete(uri"$apiUri/matcher/settings/rates/${asset.toString}")
          .contentType("application/json", "UTF-8")
          .headers(apiKeyHeaders)
      }

      override def tryRates: F[Either[MatcherError, HttpRates]] =
        tryParseJson[HttpRates](sttp.get(uri"$apiUri/matcher/settings/rates").headers(apiKeyHeaders))

      override def tryCurrentOffset: F[Either[MatcherError, HttpOffset]] = tryParse {
        sttp
          .get(uri"$apiUri/matcher/debug/currentOffset")
          .headers(apiKeyHeaders)
          .response(asLong)
      }

      override def tryLastOffset: F[Either[MatcherError, HttpOffset]] = tryParse {
        sttp
          .get(uri"$apiUri/matcher/debug/lastOffset")
          .headers(apiKeyHeaders)
          .response(asLong)
      }

      override def tryOldestSnapshotOffset: F[Either[MatcherError, HttpOffset]] = tryParse {
        sttp
          .get(uri"$apiUri/matcher/debug/oldestSnapshotOffset")
          .headers(apiKeyHeaders)
          .response(asLong)
      }

      override def tryAllSnapshotOffsets: F[Either[MatcherError, HttpSnapshotOffsets]] =
        tryParseJson(sttp.get(uri"$apiUri/matcher/debug/allSnapshotOffsets").headers(apiKeyHeaders))

      override def trySaveSnapshots: F[Either[MatcherError, Unit]] = tryUnit {
        sttp.post(uri"$apiUri/matcher/debug/saveSnapshots").headers(apiKeyHeaders)
      }

      override def waitReady: F[Unit] = {
        def request: F[Boolean] = M.handleErrorWith(tryAllOrderBooks.map(_.isRight)) {
          case _: SocketException | _: JsResultException => M.pure(false)
          case NonFatal(e)                               => M.raiseError(e)
        }

        // Sometimes container start during 20 seconds! https://github.com/docker/for-mac/issues/1183
        repeatUntil(request, RepeatRequestOptions(1.second, 60 + 20))(_ == true).map(_ => ())
      }

      override def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus] =
        repeatUntil(tryOrderStatus(assetPair, id), RepeatRequestOptions(1.second, 60)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForOrderPlacement(order: Order): F[HttpSuccessfulPlace] = repeatUntil(tryPlace(order))(_.isRight).map(_.explicitGet())

      def waitForOrderHistory[A](owner: KeyPair, activeOnly: Option[Boolean])(
          pred: List[HttpOrderBookHistoryItem] => Boolean): F[List[HttpOrderBookHistoryItem]] =
        repeatUntil[Either[MatcherError, List[HttpOrderBookHistoryItem]]](tryOrderHistory(owner, activeOnly), RepeatRequestOptions(1.second, 60)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: HttpOrderStatus.Status): F[HttpOrderStatus] =
        waitForOrder(assetPair, id)(_.status == status)

      override def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[ExchangeTransaction]] =
        waitForTransactionsByOrder(id)(_.lengthCompare(atLeast) >= 0)

      override def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]] =
        repeatUntil[Either[MatcherError, List[ExchangeTransaction]]](tryTransactionsByOrder(id), RepeatRequestOptions(1.second, 60)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForCurrentOffset(pred: Long => Boolean): F[HttpOffset] =
        repeatUntil[Either[MatcherError, HttpOffset]](tryCurrentOffset, RepeatRequestOptions(1.second, 120)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def waitForWsConnections(pred: HttpWebSocketConnections => Boolean): F[HttpWebSocketConnections] =
        repeatUntil[Either[MatcherError, HttpWebSocketConnections]](tryWsConnections, RepeatRequestOptions(1.second, 120)) {
          case Left(_)  => false
          case Right(x) => pred(x)
        }.map(_.explicitGet())

      override def trySettings: F[Either[MatcherError, HttpMatcherPublicSettings]] = tryParseJson {
        sttp
          .get(uri"$apiUri/matcher/settings")
          .headers(apiKeyHeaders)
      }

      override def tryWsConnections: F[Either[MatcherError, HttpWebSocketConnections]] = tryParseJson {
        sttp
          .get(uri"$apiUri/ws/v0/connections")
          .headers(apiKeyHeaders)
      }
      
      override def tryCloseWsConnections(oldestNumber: Int): F[Either[MatcherError, HttpMessage]] = tryParseJson {
        sttp
          .delete(uri"$apiUri/ws/v0/connections")
          .body(Json.toJson(HttpWebSocketCloseFilter(oldestNumber)).toString())
          .contentType("application/json", "UTF-8")
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
