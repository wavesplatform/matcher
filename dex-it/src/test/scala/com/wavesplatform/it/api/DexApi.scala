package com.wavesplatform.it.api

import java.net.{InetSocketAddress, SocketException}
import java.util.UUID

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.tagless.{Derive, FunctorK}
import com.google.common.primitives.Longs
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{Response, SttpBackend, MonadError => _, _}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.dex.api.CancelOrderRequest
import com.wavesplatform.dex.util.getSimpleName
import com.wavesplatform.it.api.dex.ThrowableMonadError
import com.wavesplatform.transaction.TransactionFactory
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import play.api.libs.json._

import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal

case class MatcherError(error: Int, message: String, status: String)
object MatcherError {
  implicit val format: Format[MatcherError] = Json.format[MatcherError]
}

trait DexApi[F[_]] {
  def place(order: Order): F[Unit]

  def tryCancel(owner: KeyPair, order: Order): F[Either[MatcherError, MatcherStatusResponse]] = tryCancel(owner, order.assetPair, order.id())
  def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, MatcherStatusResponse]]

  def cancel(owner: KeyPair, order: Order): F[MatcherStatusResponse] = cancel(owner, order.assetPair, order.id())
  def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[MatcherStatusResponse]

  def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Unit]
  def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Unit]

  def cancelWithApiKey(id: Order.Id): F[MatcherStatusResponse]

  def orderStatus(order: Order): F[MatcherStatusResponse] = orderStatus(order.assetPair, order.id())
  def orderStatus(assetPair: AssetPair, id: Order.Id): F[MatcherStatusResponse]

  def transactionsByOrder(id: Order.Id): F[List[exchange.ExchangeTransaction]]

  def orderHistory(owner: KeyPair, activeOnly: Option[Boolean] = None, timestamp: Long = System.currentTimeMillis()): F[List[OrderbookHistory]]
  def orderHistoryWithApiKey(owner: com.wavesplatform.account.Address, activeOnly: Option[Boolean] = None): F[List[OrderbookHistory]]

  def orderHistoryByPair(owner: KeyPair,
                         assetPair: AssetPair,
                         activeOnly: Option[Boolean] = None,
                         timestamp: Long = System.currentTimeMillis()): F[List[OrderbookHistory]]

  def orderBook(assetPair: AssetPair): F[OrderBookResponse]

  def waitReady: F[Unit]

  def waitForOrder(order: Order)(pred: MatcherStatusResponse => Boolean): F[Unit] = waitForOrder(order.assetPair, order.id())(pred)
  def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: MatcherStatusResponse => Boolean): F[Unit]

  def waitForOrderStatus(order: Order, status: OrderStatus): F[Unit] = waitForOrderStatus(order.assetPair, order.id(), status)
  def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: OrderStatus): F[Unit]

  def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[exchange.ExchangeTransaction]]
  def waitForTransactionsByOrder(id: Order.Id)(pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]]
}

sealed trait OrderStatus {
  val name = getSimpleName(this)
}

object OrderStatus {
  case object Accepted        extends OrderStatus
  case object NotFound        extends OrderStatus
  case object PartiallyFilled extends OrderStatus
  case object Filled          extends OrderStatus
  case object Cancelled       extends OrderStatus
}

object DexApi {
  implicit val functorK: FunctorK[DexApi] = Derive.functorK[DexApi]

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

      override def place(order: Order): F[Unit] = {
        val req = sttp.post(uri"$apiUri/orderbook").body(order).mapResponse(_ => ()).tag("requestId", UUID.randomUUID())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap { resp =>
          if (resp.isSuccess) M.pure(())
          else M.raiseError[Unit](new RuntimeException(s"The server returned an error: ${resp.code}"))
        }
      }

      override def tryCancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[Either[MatcherError, MatcherStatusResponse]] = {
        val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
        val req =
          sttp
            .post(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
            .body(body)
            .contentType("application/json", "UTF-8")
            .response(asJson[MatcherStatusResponse])
            .tag("requestId", UUID.randomUUID())
        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap { resp =>
          resp.rawErrorBody match {
            case Left(bytes) =>
              try M.pure(Left(Json.parse(bytes).validate[MatcherError].get))
              catch {
                case NonFatal(e) =>
                  M.raiseError[Either[MatcherError, MatcherStatusResponse]](
                    new RuntimeException(s"The server returned an error: ${resp.code}, also can't parse as MatcherError", e))
              }
            case Right(Left(error)) =>
              M.raiseError[Either[MatcherError, MatcherStatusResponse]](new RuntimeException(s"Can't parse the response: $error"))
            case Right(Right(r)) => M.pure(Right(r))
          }
        }
      }

      // repalce with tryCancel
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

      override def orderStatus(assetPair: AssetPair, id: Order.Id): F[MatcherStatusResponse] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/$id")
          .response(asJson[MatcherStatusResponse])
          .tag("requestId", UUID.randomUUID())
        repeatUntilResponse[MatcherStatusResponse](httpBackend.send(req), 1.second)(_.code == StatusCodes.Ok)
      }

      override def transactionsByOrder(id: Order.Id): F[List[exchange.ExchangeTransaction]] = {
        val req = sttp.get(uri"$apiUri/transactions/$id").response(asJson[List[exchange.ExchangeTransaction]]).tag("requestId", UUID.randomUUID())
        repeatUntilResponse[List[exchange.ExchangeTransaction]](httpBackend.send(req), 1.second)(_.code == StatusCodes.Ok)
      }

      override def orderHistory(owner: KeyPair,
                                activeOnly: Option[Boolean] = None,
                                timestamp: Long = System.currentTimeMillis()): F[List[OrderbookHistory]] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${Base58.encode(owner.publicKey)}${activeOnly.fold("")(x => s"?activeOnly=$x}")}")
          .headers(timestampAndSignatureHeaders(owner, timestamp))
          .response(asJson[List[OrderbookHistory]])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def orderHistoryWithApiKey(owner: com.wavesplatform.account.Address, activeOnly: Option[Boolean] = None): F[List[OrderbookHistory]] = {
        val req = sttp
          .get(uri"$apiUri/orders/${owner.stringRepr}${activeOnly.fold("")(x => s"?activeOnly=$x}")}")
          .headers(apiKeyHeaders)
          .response(asJson[List[OrderbookHistory]])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def orderHistoryByPair(owner: KeyPair,
                                      assetPair: AssetPair,
                                      activeOnly: Option[Boolean] = None,
                                      timestamp: Long = System.currentTimeMillis()): F[List[OrderbookHistory]] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/publicKey/${Base58
            .encode(owner.publicKey)}${activeOnly.fold("")(x => s"?activeOnly=$x}")}")
          .headers(timestampAndSignatureHeaders(owner, timestamp))
          .response(asJson[List[OrderbookHistory]])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def orderBook(assetPair: AssetPair): F[OrderBookResponse] = {
        val req = sttp
          .get(uri"$apiUri/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
          .response(asJson[OrderBookResponse])
          .tag("requestId", UUID.randomUUID())

        repeatUntil(httpBackend.send(req), 1.second)(resp => resp.isSuccess || resp.isClientError).flatMap(parseResponse)
      }

      override def waitReady: F[Unit] = {
        val req = sttp.get(uri"$apiUri").mapResponse(_ => ())

        def loop(): F[Response[Unit]] = M.handleErrorWith(httpBackend.send(req)) {
          case _: SocketException => W.wait(1.second).flatMap(_ => loop())
          case NonFatal(e)        => M.raiseError(e)
        }

        repeatUntil(loop(), 1.second)(_.code == StatusCodes.Ok).map(_ => ())
      }

      override def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: MatcherStatusResponse => Boolean): F[Unit] =
        repeatUntil(orderStatus(assetPair, id), 1.second)(pred).map(_ => ())

      override def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: OrderStatus): F[Unit] =
        waitForOrder(assetPair, id)(_.status == status.name)

      override def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[exchange.ExchangeTransaction]] =
        waitForTransactionsByOrder(id)(_.lengthCompare(atLeast) >= 0)

      override def waitForTransactionsByOrder(id: Order.Id)(
          pred: List[exchange.ExchangeTransaction] => Boolean): F[List[exchange.ExchangeTransaction]] =
        repeatUntil[List[exchange.ExchangeTransaction]](transactionsByOrder(id), 1.second)(pred)

      private def timestampAndSignatureHeaders(owner: KeyPair, timestamp: Long): Map[String, String] = Map(
        "Timestamp" -> timestamp.toString,
        "Signature" -> Base58.encode(crypto.sign(owner, owner.publicKey ++ Longs.toByteArray(timestamp)))
      )

      private def apiKeyHeaders: Map[String, String] = Map("X-API-Key" -> apiKey)
    }
}
