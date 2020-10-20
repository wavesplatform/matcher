package com.wavesplatform.dex.it.api.dex

import cats.tagless._
import com.typesafe.config.Config
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import im.mak.waves.transactions.ExchangeTransaction

@finalAlg
@autoFunctorK
trait DexApi[F[_]] {
  def publicKey: F[HttpMatcherPublicKey]

  def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpBalance]
  def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey] = None): F[HttpBalance]

  def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis): F[HttpBalance]

  def place(order: Order): F[HttpSuccessfulPlace]
  def placeMarket(order: Order): F[HttpSuccessfulPlace]

  def cancel(owner: KeyPair, order: Order): F[HttpSuccessfulSingleCancel] = cancel(owner, order.assetPair, order.id())
  def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[HttpSuccessfulSingleCancel]

  def cancelWithApiKey(order: Order, xUserPublicKey: Option[PublicKey] = None): F[HttpSuccessfulSingleCancel] =
    cancelWithApiKey(order.id(), xUserPublicKey)

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
    xUserPublicKey: Option[PublicKey] = None
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
    order: Order,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpOrderBookHistoryItem] =
    orderStatusInfoByIdWithSignature(owner, order.id(), timestamp)

  def orderStatusInfoByIdWithSignature(
    owner: KeyPair,
    orderId: Order.Id,
    timestamp: Long
  ): F[HttpOrderBookHistoryItem]

  def transactionsByOrder(order: Order): F[List[ExchangeTransaction]] = transactionsByOrder(order.id())
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
  def saveSnapshots: F[HttpMessage]

  def settings: F[HttpMatcherPublicSettings]
  def config: F[Config]

  def wsConnections: F[HttpWebSocketConnections]
  def closeWsConnections(oldestNumber: Int): F[HttpMessage]
}

object DexApi {} // Without this line we have java.lang.NoClassDefFoundError: com/wavesplatform/dex/it/dex/DexApi$
