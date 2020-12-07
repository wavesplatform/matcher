package com.wavesplatform.dex.it.api.dex

import cats.tagless._
import com.typesafe.config.Config
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import im.mak.waves.transactions.ExchangeTransaction
import play.api.libs.json.JsObject

@finalAlg
@autoFunctorK
trait DexApi[F[_]] {
  def publicKey: F[HttpMatcherPublicKey]

  def getReservedBalance(publicKey: String, timestamp: Long, signature: String): F[HttpBalance]
  def getReservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpBalance]
  def getReservedBalance(publicKey: String, headers: Map[String, String]): F[HttpBalance]
  def getReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey] = None): F[HttpBalance]

  def getTradableBalance(address: String, amountAsset: String, priceAsset: String): F[HttpBalance]
  def getTradableBalance(of: KeyPair, assetPair: AssetPair): F[HttpBalance]

  def place(order: Order): F[HttpSuccessfulPlace]
  def place(order: JsObject): F[HttpSuccessfulPlace]

  def placeMarket(order: JsObject): F[HttpSuccessfulPlace]
  def placeMarket(order: Order): F[HttpSuccessfulPlace]

  def cancelOrder(owner: KeyPair, amountAsset: String, priceAsset: String, orderId: String, timestamp: Long, signature: ByteStr): F[HttpSuccessfulSingleCancel]
  def cancelOrder(owner: KeyPair, order: Order): F[HttpSuccessfulSingleCancel] = cancelOrder(owner, order.assetPair, order.id())
  def cancelOrder(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[HttpSuccessfulSingleCancel]

  def cancelOrderById(id: String, headers: Map[String, String]): F[HttpSuccessfulSingleCancel]

  def cancelOrderById(order: Order, xUserPublicKey: Option[PublicKey] = None): F[HttpSuccessfulSingleCancel] =
    cancelOrderById(order.id(), xUserPublicKey)

  def cancelOrderById(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[HttpSuccessfulSingleCancel]

  def cancelAll(sender: KeyPair, timestamp: Long, signature: ByteStr): F[HttpSuccessfulBatchCancel]
  def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpSuccessfulBatchCancel]

  def cancelAllByPair(
    owner: KeyPair,
    assetPair: AssetPair,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpSuccessfulBatchCancel]

  def cancelAllByAddressAndIds(address: String, ids: Set[String]): F[HttpSuccessfulBatchCancel]

  def cancelAllByAddressAndIds(address: String, ids: Set[String], headers: Map[String, String]): F[HttpSuccessfulBatchCancel]

  def cancelAllByApiKeyAndIds(
    owner: Address,
    orderIds: Set[Order.Id],
    xUserPublicKey: Option[PublicKey] = None
  ): F[HttpSuccessfulBatchCancel]

  def getOrderStatus(order: Order): F[HttpOrderStatus] = getOrderStatus(order.assetPair, order.id())

  def getOrderStatus(assetPair: AssetPair, id: Order.Id): F[HttpOrderStatus] =
    getOrderStatus(assetPair.amountAssetStr, assetPair.priceAssetStr, id.toString)

  def getOrderStatus(amountAsset: String, priceAsset: String, id: String): F[HttpOrderStatus]

  def getOrderStatusInfoById(
    address: String,
    orderId: String,
    headers: Map[String, String] = Map.empty
  ): F[HttpOrderBookHistoryItem]

  def getOrderStatusInfoByIdWithApiKey(
    owner: Address,
    orderId: Order.Id,
    xUserPublicKey: Option[PublicKey]
  ): F[HttpOrderBookHistoryItem]

  def getOrderStatusInfoByIdWithSignature(publicKey: String, orderId: String, timestamp: Long, signature: String): F[HttpOrderBookHistoryItem]

  def getOrderStatusInfoByIdWithSignature(publicKey: String, orderId: String, headers: Map[String, String]): F[HttpOrderBookHistoryItem]

  def getOrderStatusInfoByIdWithSignature(
    owner: KeyPair,
    order: Order,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpOrderBookHistoryItem] =
    getOrderStatusInfoByIdWithSignature(owner, order.id(), timestamp)

  def getOrderStatusInfoByIdWithSignature(
    owner: KeyPair,
    orderId: Order.Id,
    timestamp: Long
  ): F[HttpOrderBookHistoryItem]

  def getTransactionsByOrder(orderId: String): F[List[ExchangeTransaction]] = getTransactionsByOrder(orderId)

  def getTransactionsByOrder(order: Order): F[List[ExchangeTransaction]] = getTransactionsByOrder(order.id())

  def getTransactionsByOrder(id: Order.Id): F[List[ExchangeTransaction]]

  def deleteHistory(owner: KeyPair, assetPair: AssetPair, orderId: String): F[HttpSuccessfulDeleteHistory]

  def getOrderHistoryByApiKey(publicKey: String, timestamp: Long, signature: String): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByApiKey(address: String): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByApiKey(
    address: String,
    activeOnly: Boolean,
    closedOnly: Boolean,
    headers: Map[String, String]
  ): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByPublicKey(owner: KeyPair): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def getOrderHistoryByPublicKey(
    publicKey: String,
    timestamp: Long,
    signature: String
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def getOrderHistoryByPublicKey(
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

  def getOrderHistoryByAssetPairAndPublicKey(
    publicKey: String,
    amountAsset: String,
    priceAsset: String,
    timestamp: Long,
    signature: String
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def getOrderHistoryByAssetPairAndPublicKey(
    owner: KeyPair,
    assetPair: AssetPair,
    activeOnly: Option[Boolean] = None,
    closedOnly: Option[Boolean] = None,
    timestamp: Long = System.currentTimeMillis
  ): F[List[HttpOrderBookHistoryItem]]

  def getOrderBooks: F[HttpTradingMarkets]

  def getOrderBook(amountAsset: String, priceAsset: String): F[HttpV0OrderBook]
  def getOrderBook(assetPair: AssetPair): F[HttpV0OrderBook]
  def getOrderBook(assetPair: AssetPair, depth: String): F[HttpV0OrderBook]
  def getOrderBook(assetPair: AssetPair, depth: Int): F[HttpV0OrderBook]

  def getOrderBookInfo(assetPair: AssetPair): F[HttpOrderBookInfo]
  def getOrderBookInfo(amountAsset: String, priceAsset: String): F[HttpOrderBookInfo]

  def getOrderBookStatus(amountAsset: String, priceAsset: String): F[HttpOrderBookStatus]
  def getOrderBookStatus(assetPair: AssetPair): F[HttpOrderBookStatus]

  def deleteOrderBook(amountAsset: String, priceAsset: String, headers: Map[String, String]): F[HttpMessage]
  def deleteOrderBook(assetPair: AssetPair): F[HttpMessage]

  def upsertRate(assetId: String, rate: Double, headers: Map[String, String] = Map.empty): F[HttpMessage]
  def upsertRate(asset: Asset, rate: Double): F[HttpMessage]
  def upsertRate(asset: Asset, rate: String): F[HttpMessage]
  def deleteRate(assetId: String, headers: Map[String, String] = Map.empty): F[HttpMessage]
  def deleteRate(asset: Asset): F[HttpMessage]
  def getRates: F[HttpRates]

  def getCurrentOffset: F[HttpOffset]
  def getCurrentOffset(headers: Map[String, String]): F[HttpOffset]

  def getLastOffset: F[HttpOffset]
  def getLastOffset(headers: Map[String, String]): F[HttpOffset]

  def getOldestSnapshotOffset: F[HttpOffset]
  def getOldestSnapshotOffset(headers: Map[String, String]): F[HttpOffset]

  def getAllSnapshotOffsets(headers: Map[String, String]): F[HttpSnapshotOffsets]
  def getAllSnapshotOffsets: F[HttpSnapshotOffsets]

  def saveSnapshots(headers: Map[String, String]): F[HttpMessage]
  def saveSnapshots: F[HttpMessage]

  def getMatcherSettings: F[HttpMatcherPublicSettings]
  def getMatcherConfig: F[Config]
  def getMatcherConfig(headers: Map[String, String]): F[Config]

  def getMatcherPublicKey: F[String]

  def print(message: String): F[Unit]

  def wsConnections: F[HttpWebSocketConnections]
  def closeWsConnections(oldestNumber: Int): F[HttpMessage]
}

object DexApi {} // Without this line we have java.lang.NoClassDefFoundError: com/wavesplatform/dex/it/dex/DexApi$
