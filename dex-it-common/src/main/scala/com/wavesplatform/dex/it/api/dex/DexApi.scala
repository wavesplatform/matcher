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

  def getReservedBalanceByPK(publicKey: String, timestamp: Long, signature: String): F[HttpBalance]
  def getReservedBalanceByPK(of: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpBalance]
  def getReservedBalanceByPK(publicKey: String, headers: Map[String, String]): F[HttpBalance]
  def getReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey] = None): F[HttpBalance]

  def getTradableBalanceByAssetPairAndAddress(address: String, amountAsset: String, priceAsset: String): F[HttpBalance]
  def getTradableBalanceByAssetPairAndAddress(of: KeyPair, assetPair: AssetPair): F[HttpBalance]

  def place(order: Order): F[HttpSuccessfulPlace]
  def place(order: JsObject): F[HttpSuccessfulPlace]

  def placeMarket(order: JsObject): F[HttpSuccessfulPlace]
  def placeMarket(order: Order): F[HttpSuccessfulPlace]

  def cancelOneOrAllInPairOrdersWithSig(
    owner: KeyPair,
    amountAsset: String,
    priceAsset: String,
    orderId: String,
    timestamp: Long,
    signature: ByteStr
  ): F[HttpSuccessfulSingleCancel]

  def cancelOneOrAllInPairOrdersWithSig(owner: KeyPair, order: Order): F[HttpSuccessfulSingleCancel] =
    cancelOneOrAllInPairOrdersWithSig(owner, order.assetPair, order.id())

  def cancelOneOrAllInPairOrdersWithSig(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[HttpSuccessfulSingleCancel]

  def cancelOneOrderWithKey(id: String, headers: Map[String, String]): F[HttpSuccessfulSingleCancel]

  def cancelOrderById(order: Order, xUserPublicKey: Option[PublicKey] = None): F[HttpSuccessfulSingleCancel] =
    cancelOneOrderWithKey(order.id(), xUserPublicKey)

  def cancelOneOrderWithKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[HttpSuccessfulSingleCancel]

  def cancelAllOrdersWithSig(sender: KeyPair, timestamp: Long, signature: ByteStr): F[HttpSuccessfulBatchCancel]
  def cancelAllOrdersWithSig(owner: KeyPair, timestamp: Long = System.currentTimeMillis): F[HttpSuccessfulBatchCancel]

  def cancelOneOrAllInPairOrdersWithSig(
    owner: KeyPair,
    assetPair: AssetPair,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpSuccessfulBatchCancel]

  def cancelOrdersByIdsWithKey(address: String, ids: Set[String]): F[HttpSuccessfulBatchCancel]

  def cancelOrdersByIdsWithKey(address: String, ids: Set[String], headers: Map[String, String]): F[HttpSuccessfulBatchCancel]

  def cancelOrdersByIdsWithKey(
    owner: Address,
    orderIds: Set[Order.Id],
    xUserPublicKey: Option[PublicKey] = None
  ): F[HttpSuccessfulBatchCancel]

  def orderStatusByAssetPairAndId(order: Order): F[HttpOrderStatus] = orderStatusByAssetPairAndId(order.assetPair, order.id())

  def orderStatusByAssetPairAndId(assetPair: AssetPair, id: Order.Id): F[HttpOrderStatus] =
    getOrderStatusByAssetPairAndId(assetPair.amountAssetStr, assetPair.priceAssetStr, id.toString)

  def getOrderStatusByAssetPairAndId(amountAsset: String, priceAsset: String, id: String): F[HttpOrderStatus]

  def getOrderStatusByAddressAndIdWithKey(
    address: String,
    orderId: String,
    headers: Map[String, String] = Map.empty
  ): F[HttpOrderBookHistoryItem]

  def getOrderStatusByAddressAndIdWithKey(
    owner: Address,
    orderId: Order.Id,
    xUserPublicKey: Option[PublicKey]
  ): F[HttpOrderBookHistoryItem]

  def getOrderStatusByPKAndIdWithSig(publicKey: String, orderId: String, timestamp: Long, signature: String): F[HttpOrderBookHistoryItem]

  def getOrderStatusByPKAndIdWithSig(publicKey: String, orderId: String, headers: Map[String, String]): F[HttpOrderBookHistoryItem]

  def getOrderStatusByPKAndIdWithSig(
    owner: KeyPair,
    order: Order,
    timestamp: Long = System.currentTimeMillis
  ): F[HttpOrderBookHistoryItem] =
    getOrderStatusByPKAndIdWithSig(owner, order.id(), timestamp)

  def getOrderStatusByPKAndIdWithSig(
    owner: KeyPair,
    orderId: Order.Id,
    timestamp: Long
  ): F[HttpOrderBookHistoryItem]

  def getTransactionsByOrderId(orderId: String): F[List[ExchangeTransaction]] = getTransactionsByOrderId(orderId)

  def getTransactionsByOrderId(order: Order): F[List[ExchangeTransaction]] = getTransactionsByOrderId(order.id())

  def getTransactionsByOrderId(id: Order.Id): F[List[ExchangeTransaction]]

  def deleteOrderFromHistoryById(owner: KeyPair, assetPair: AssetPair, orderId: String): F[HttpSuccessfulDeleteHistory]

  def getOrderHistoryByAddressWithKey(publicKey: String, timestamp: Long, signature: String): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByAddressWithKey(address: String): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByAddressWithKey(
    address: String,
    activeOnly: Boolean,
    closedOnly: Boolean,
    headers: Map[String, String]
  ): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByPKWithSig(owner: KeyPair): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def getOrderHistoryByPKWithSig(
    publicKey: String,
    timestamp: Long,
    signature: String
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def getOrderHistoryByPKWithSig(
    owner: KeyPair,
    activeOnly: Option[Boolean] = None,
    closedOnly: Option[Boolean] = None,
    timestamp: Long = System.currentTimeMillis
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as true if it wasn't specified
   */
  def orderHistoryByAddressWithKey(
    owner: Address,
    activeOnly: Option[Boolean] = None,
    closedOnly: Option[Boolean] = None,
    xUserPublicKey: Option[PublicKey] = None
  ): F[List[HttpOrderBookHistoryItem]]

  def getOrderHistoryByAssetPairAndPKWithSig(
    publicKey: String,
    amountAsset: String,
    priceAsset: String,
    timestamp: Long,
    signature: String
  ): F[List[HttpOrderBookHistoryItem]]

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  def getOrderHistoryByAssetPairAndPKWithSig(
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

  def getOrderBookRestrictions(assetPair: AssetPair): F[HttpOrderBookInfo]
  def getOrderBookRestrictions(amountAsset: String, priceAsset: String): F[HttpOrderBookInfo]

  def getOrderBookStatus(amountAsset: String, priceAsset: String): F[HttpOrderBookStatus]
  def getOrderBookStatus(assetPair: AssetPair): F[HttpOrderBookStatus]

  def deleteOrderBookWithKey(amountAsset: String, priceAsset: String, headers: Map[String, String]): F[HttpMessage]
  def deleteOrderBookWithKey(assetPair: AssetPair): F[HttpMessage]

  def upsertAssetRate(assetId: String, rate: Double, headers: Map[String, String] = Map.empty): F[HttpMessage]
  def upsertAssetRate(asset: Asset, rate: Double): F[HttpMessage]
  def upsertAssetRate(asset: Asset, rate: String): F[HttpMessage]
  def deleteAssetRate(assetId: String, headers: Map[String, String] = Map.empty): F[HttpMessage]
  def deleteAssetRate(asset: Asset): F[HttpMessage]
  def getAssetRates: F[HttpRates]

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

  def getMatcherPublicSettings: F[HttpMatcherPublicSettings]
  def getMatcherConfig: F[Config]
  def getMatcherConfig(headers: Map[String, String]): F[Config]

  def getAddressState(address: Address): F[HttpAddressState]
  def getAddressState(address: String): F[HttpAddressState]
  def getAddressState(address: Address, headers: Map[String, String]): F[HttpAddressState]
  def getAddressState(address: String, headers: Map[String, String]): F[HttpAddressState]

  def getMatcherStatus: F[HttpSystemStatus]
  def getMatcherStatus(headers: Map[String, String]): F[HttpSystemStatus]

  def getMatcherPKInBase58: F[String]

  def printMessage(message: String): F[Unit]

  def wsConnections: F[HttpWebSocketConnections]
  def closeWsConnections(oldestNumber: Int): F[HttpMessage]
}

object DexApi {} // Without this line we have java.lang.NoClassDefFoundError: com/wavesplatform/dex/it/dex/DexApi$
