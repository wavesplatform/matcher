package com.wavesplatform.dex.it.dex

import cats._
import cats.implicits._
import cats.tagless._
import com.softwaremill.sttp.Response
import com.typesafe.config.Config
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.api.EnrichedResponse
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import im.mak.waves.transactions.ExchangeTransaction
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

@finalAlg
@autoFunctorK
trait DexNewApi[F[_]] {
  def publicKey: F[HttpMatcherPublicKey]

  def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpBalance]
  def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): F[HttpBalance]

  def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis): F[HttpBalance]

  def place(order: Order): F[HttpSuccessfulPlace]
  def placeMarket(order: Order): F[HttpSuccessfulPlace]

  def cancel(owner: KeyPair, order: Order): F[HttpSuccessfulSingleCancel] = cancel(owner, order.assetPair, order.id())
  def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[HttpSuccessfulSingleCancel]
  def cancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[HttpSuccessfulSingleCancel]

  def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpSuccessfulBatchCancel]

  def cancelAllByPair(
    owner: KeyPair,
    assetPair: AssetPair,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpSuccessfulBatchCancel]

  def cancelAllByIdsWithApiKey(
    owner: Address,
    orderIds: Set[Order.Id],
    xUserPublicKey: Option[PublicKey]
  ): F[HttpSuccessfulBatchCancel]

  def orderStatus(order: Order): F[HttpOrderStatus] = orderStatus(order.assetPair, order.id())
  def orderStatus(assetPair: AssetPair, id: Order.Id): F[HttpOrderStatus]

  def orderStatusInfoByIdWithApiKey(
    owner: Address,
    orderId: Order.Id,
    xUserPublicKey: Option[PublicKey]
  ): F[HttpOrderBookHistoryItem]

  def orderStatusInfoByIdWithSignature(
    owner: KeyPair,
    orderId: Order.Id,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpOrderBookHistoryItem]

  def transactionsByOrder(id: Order.Id): F[List[ExchangeTransaction]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def orderHistory(
    owner: KeyPair,
    activeOnly: Option[Boolean] = None,
    closedOnly: Option[Boolean] = None,
    timestamp: Long = System.currentTimeMillis
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as true if it wasn't specified
   */
  def orderHistoryWithApiKey(
    owner: Address,
    activeOnly: Option[Boolean] = None,
    closedOnly: Option[Boolean] = None,
    xUserPublicKey: Option[PublicKey] = None
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def orderHistoryByPair(
    owner: KeyPair,
    assetPair: AssetPair,
    activeOnly: Option[Boolean] = None,
    closedOnly: Option[Boolean] = None,
    timestamp: Long = System.currentTimeMillis
  ): F[List[HttpOrderBookHistoryItem]]

  def allOrderBooks: F[HttpTradingMarkets]

  def orderBook(assetPair: AssetPair): F[HttpV0OrderBook]
  def orderBook(assetPair: AssetPair, depth: Int): F[HttpV0OrderBook]

  def orderBookInfo(assetPair: AssetPair): F[HttpOrderBookInfo]
  def orderBookStatus(assetPair: AssetPair): F[HttpMarketStatus]

  def deleteOrderBook(assetPair: AssetPair): F[HttpMessage]

  def upsertRate(asset: Asset, rate: Double): F[HttpMessage]
  def deleteRate(asset: Asset): F[HttpMessage]
  def rates: F[HttpRates]

  def currentOffset: F[HttpOffset]
  def lastOffset: F[HttpOffset]
  def oldestSnapshotOffset: F[HttpOffset]
  def allSnapshotOffsets: F[HttpSnapshotOffsets]
  def saveSnapshots: F[Unit]

  def settings: F[HttpMatcherPublicSettings]
  def config: F[Config]

  def wsConnections: F[HttpWebSocketConnections]
  def closeWsConnections(oldestNumber: Int): F[HttpMessage]

  // TODO move

  def waitForOrder(order: Order)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus] = waitForOrder(order.assetPair, order.id())(pred)
  def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus]

  def waitForOrderPlacement(order: Order): F[HttpSuccessfulPlace]

  def waitForOrderHistory[A](owner: KeyPair, activeOnly: Option[Boolean])(
    pred: List[HttpOrderBookHistoryItem] => Boolean
  ): F[List[HttpOrderBookHistoryItem]]

  def waitForOrderStatus(order: Order, status: HttpOrderStatus.Status): F[HttpOrderStatus] =
    waitForOrderStatus(order.assetPair, order.id(), status)

  def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: HttpOrderStatus.Status): F[HttpOrderStatus]

  def waitForTransactionsByOrder(order: Order, atLeast: Int): F[List[ExchangeTransaction]] = waitForTransactionsByOrder(order.id(), atLeast)
  def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[ExchangeTransaction]]

  def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]]

  def waitForCurrentOffset(pred: Long => Boolean): F[HttpOffset]

  def waitForWsConnections(pred: HttpWebSocketConnections => Boolean): F[HttpWebSocketConnections]
}

object DexNewApi {

  type AsyncEnriched[T] = Future[EnrichedResponse[T]]

  type AsyncHttpResponse[_] = Future[Response[String]]
  type AsyncTry[T] = Future[Either[MatcherError, T]]
  type AsyncUnsafe[T] = Future[T]

  type SyncTry[T] = Either[MatcherError, T]
  type SyncUnsafe[T] = T

  implicit val toAsyncHttpResponse: AsyncEnriched ~> AsyncHttpResponse = λ[AsyncEnriched ~> AsyncHttpResponse](_.map(_.response))
  implicit val toAsyncTry: AsyncEnriched ~> AsyncTry = λ[AsyncEnriched ~> AsyncTry](_.map(parseWithMatcherError))
  implicit val toAsyncUnsafe: AsyncEnriched ~> AsyncUnsafe = λ[AsyncEnriched ~> AsyncUnsafe](_.map(parseUnsafe))

  val syncTimeout = 10.minutes

  implicit val toSyncTry: AsyncEnriched ~> SyncTry = λ[AsyncEnriched ~> SyncTry] { x =>
    Await.result(x.map(parseWithMatcherError), syncTimeout)
  }

  implicit val toSyncUnsafe: AsyncEnriched ~> SyncUnsafe = λ[AsyncEnriched ~> SyncUnsafe] { x =>
    Await.result(x.map(parseUnsafe), syncTimeout)
  }

  def parseUnsafe[T](enriched: EnrichedResponse[T]): T = parseWithMatcherError(enriched) match {
    case Left(e) => throw new RuntimeException(s"An unexpected MatcherError: $e")
    case Right(x) => x
  }

  def parseWithMatcherError[T](enriched: EnrichedResponse[T]): Either[MatcherError, T] = enriched.response.body match {
    case Left(e) =>
      Json.parse(e)
        .asOpt[MatcherError]
        .fold(throw new RuntimeException(s"The server returned error, but can't parse response as MatcherError: $e"))(identity)
        .asLeft
    case Right(x) =>
      Json.parse(x)
        .asOpt(enriched.reads)
        .fold(throw new RuntimeException(s"The server returned success, but can't parse response: $x"))(identity)
        .asRight
  }

}
