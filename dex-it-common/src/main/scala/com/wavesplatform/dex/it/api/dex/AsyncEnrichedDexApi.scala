package com.wavesplatform.dex.it.api.dex

import com.google.common.primitives.Longs
import com.typesafe.config.Config
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.it.api._
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.json._
import im.mak.waves.transactions.ExchangeTransaction
import play.api.libs.json.{JsObject, Json}
import sttp.client3._
import sttp.client3.playJson._
import sttp.model.Uri.QuerySegment
import sttp.model.{MediaType, Uri}

import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedDexApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Any])
    extends AsyncEnrichedApi[MatcherError](host)
    with DexApi[AsyncEnrichedDexApi.R] {

  override def publicKey: R[HttpMatcherPublicKey] = mk(basicRequest.get(uri"$apiUri/matcher"))

  override def getReservedBalanceByPK(publicKey: String, timestamp: Long, signature: String): R[HttpBalance] =
    getReservedBalanceByPK(publicKey, Map("timestamp" -> timestamp.toString, "signature" -> signature))

  override def getReservedBalanceByPK(of: KeyPair, timestamp: Long): R[HttpBalance] =
    getReservedBalanceByPK(Base58.encode(of.publicKey), timestampAndSignatureHeaders(of, timestamp))

  override def getReservedBalanceByPK(publicKey: String, headers: Map[String, String]): R[HttpBalance] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/balance/reserved/$publicKey")
      .headers(headers)
  }

  override def getReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): R[HttpBalance] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/balance/reserved/${Base58.encode(of.publicKey)}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  override def getTradableBalanceByAssetPairAndAddress(address: String, amountAsset: String, priceAsset: String): R[HttpBalance] = mk {
    basicRequest
      .get(
        uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/tradableBalance/$address"
      )
      .followRedirects(false)
  }

  override def getTradableBalanceByAssetPairAndAddress(of: KeyPair, assetPair: AssetPair): R[HttpBalance] =
    getTradableBalanceByAssetPairAndAddress(of.publicKey.toAddress.stringRepr, assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def place(order: Order): R[HttpSuccessfulPlace] = mk {
    basicRequest
      .body(order)
      .post(uri"$apiUri/matcher/orderbook")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false) // TODO move ?
  }

  override def place(order: JsObject): R[HttpSuccessfulPlace] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orderbook")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false) // TODO move ?
      .body(order)
  }

  override def placeMarket(order: JsObject): R[HttpSuccessfulPlace] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/market")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false) // TODO move ?
      .body(order)
  }

  override def placeMarket(order: Order): R[HttpSuccessfulPlace] = mk {
    basicRequest.post(uri"$apiUri/matcher/orderbook/market").body(order)
  }

  override def cancelOneOrAllInPairOrdersWithSig(
    owner: KeyPair,
    amountAsset: String,
    priceAsset: String,
    orderId: String,
    timestamp: Long,
    signature: ByteStr
  ): R[HttpSuccessfulSingleCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/cancel")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false)
      .body(Json.stringify(Json.toJson(cancelRequest(owner, orderId).copy(signature = signature))))
      .contentType(MediaType.ApplicationJson)
  }

  override def cancelOneOrAllInPairOrdersWithSig(owner: KeyPair, assetPair: AssetPair, id: Id): R[HttpSuccessfulSingleCancel] = mk {
    val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false)
      .body(body)
      .contentType(MediaType.ApplicationJson)
  }

  override def cancelOneOrderWithKey(id: String, headers: Map[String, String]): R[HttpSuccessfulSingleCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orders/cancel/$id")
      .headers(headers)
      .contentType(MediaType.ApplicationJson)
  }

  override def cancelOneOrderWithKey(id: Id, xUserPublicKey: Option[PublicKey]): R[HttpSuccessfulSingleCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orders/cancel/${id.toString}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
      .contentType(MediaType.ApplicationJson)
  }

  override def cancelAllOrdersWithSig(sender: KeyPair, timestamp: Long, signature: ByteStr): R[HttpSuccessfulBatchCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/cancel")
      .body(Json.stringify(Json.toJson(batchCancelRequest(sender, timestamp).copy(signature = signature))))
      .contentType(MediaType.ApplicationJson)
  }

  override def cancelAllOrdersWithSig(owner: KeyPair, timestamp: Long): R[HttpSuccessfulBatchCancel] = mk {
    val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/cancel")
      .body(body)
      .contentType(MediaType.ApplicationJson)
  }

  override def cancelOneOrAllInPairOrdersWithSig(owner: KeyPair, assetPair: AssetPair, timestamp: Long): R[HttpSuccessfulBatchCancel] = mk {
    val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
      .body(body)
      .contentType(MediaType.ApplicationJson)
  }

  def cancelOrdersByIdsWithKey(
    address: String,
    ids: Set[String],
    headers: Map[String, String]
  ): R[HttpSuccessfulBatchCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orders/$address/cancel")
      .headers(headers)
      .body(Json.stringify(Json.toJson(ids)))
      .contentType(MediaType.ApplicationJson)
  }

  def cancelOrdersByIdsWithKey(address: String, ids: Set[String]): R[HttpSuccessfulBatchCancel] =
    cancelOrdersByIdsWithKey(address, ids, apiKeyHeaders)

  def cancelOrdersByIdsWithKey(owner: Address, orderIds: Set[Order.Id], headers: Map[String, String]): R[HttpSuccessfulBatchCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orders/$owner/cancel")
      .headers(headers)
      .body(Json.stringify(Json.toJson(orderIds)))
      .contentType(MediaType.ApplicationJson)
  }

  def cancelOrdersByIdsWithKey(owner: Address, orderIds: Set[Order.Id]): R[HttpSuccessfulBatchCancel] =
    cancelOrdersByIdsWithKey(owner, orderIds, apiKeyHeaders)

  override def cancelOrdersByIdsWithKey(
    owner: Address,
    orderIds: Set[Id],
    xUserPublicKey: Option[PublicKey] = None
  ): R[HttpSuccessfulBatchCancel] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orders/$owner/cancel")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
      .body(Json.stringify(Json.toJson(orderIds)))
      .contentType(MediaType.ApplicationJson)
  }

  override def getOrderStatusByAssetPairAndId(amountAsset: String, priceAsset: String, id: String): R[HttpOrderStatus] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/$id")
      .readTimeout(3.minutes) // TODO find way to decrease timeout!
      .followRedirects(false)
  }

  override def getOrderStatusByAddressAndIdWithKey(
    address: String,
    orderId: String,
    headers: Map[String, String] = Map.empty
  ): R[HttpOrderBookHistoryItem] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orders/$address/$orderId")
      .headers(headers)
  }

  override def getOrderStatusByAddressAndIdWithKey(
    owner: Address,
    orderId: Id,
    xUserPublicKey: Option[PublicKey]
  ): R[HttpOrderBookHistoryItem] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orders/$owner/${orderId.toString}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  override def getOrderStatusByPKAndIdWithSig(publicKey: String, orderId: String, headers: Map[String, String]): R[HttpOrderBookHistoryItem] =
    mk {
      basicRequest
        .get(uri"$apiUri/matcher/orderbook/$publicKey/$orderId")
        .headers(headers)
    }

  override def getOrderStatusByPKAndIdWithSig(
    publicKey: String,
    orderId: String,
    timestamp: Long,
    signature: String
  ): R[HttpOrderBookHistoryItem] =
    getOrderStatusByPKAndIdWithSig(publicKey, orderId, Map("timestamp" -> timestamp.toString, "signature" -> signature))

  override def getOrderStatusByPKAndIdWithSig(owner: KeyPair, orderId: Id, timestamp: Long): R[HttpOrderBookHistoryItem] =
    getOrderStatusByPKAndIdWithSig(Base58.encode(owner.publicKey), orderId.toString, timestampAndSignatureHeaders(owner, timestamp))

  override def getTransactionsByOrderId(orderId: String): R[List[ExchangeTransaction]] = mk {
    basicRequest.get(uri"$apiUri/matcher/transactions/$orderId")
  }

  override def getTransactionsByOrderId(id: Id): R[List[ExchangeTransaction]] = getTransactionsByOrderId(id.toString)

  override def deleteOrderFromHistoryById(
    owner: KeyPair,
    assetPair: AssetPair,
    orderId: String
  ): R[HttpSuccessfulDeleteHistory] = mk {
    basicRequest
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/delete")
      .followRedirects(false)
      .body(Json.stringify(Json.toJson(cancelRequest(owner, orderId))))
      .contentType(MediaType.ApplicationJson)

  }

  override def getOrderHistoryByPKWithSig(publicKey: String, timestamp: Long, signature: String): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/$publicKey")
      .headers(Map("timestamp" -> timestamp.toString, "signature" -> signature))
  }

  override def getOrderHistoryByAddressWithKey(publicKey: String, timestamp: Long, signature: String): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orders/$publicKey")
      .headers(Map("timestamp" -> timestamp.toString, "signature" -> signature))
  }

  override def getOrderHistoryByPKWithSig(owner: KeyPair): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/${Base58.encode(owner.publicKey)}")
      .headers(timestampAndSignatureHeaders(owner, System.currentTimeMillis()))
  }

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  override def getOrderHistoryByPKWithSig(
    owner: KeyPair,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    timestamp: Long
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(appendFilters(uri"$apiUri/matcher/orderbook/${Base58.encode(owner.publicKey)}", activeOnly, closedOnly))
      .headers(timestampAndSignatureHeaders(owner, timestamp))
  }

  override def getOrderHistoryByAddressWithKey(address: String): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orders/$address")
      .headers(apiKeyHeaders)
  }

  override def getOrderHistoryByAddressWithKey(
    address: String,
    activeOnly: Boolean,
    closedOnly: Boolean,
    headers: Map[String, String]
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orders/$address?activeOnly=$activeOnly&closedOnly=$closedOnly")
      .headers(headers)
  }

  /**
   * param @activeOnly Server treats this parameter as true if it wasn't specified
   */
  override def orderHistoryByAddressWithKey(
    owner: Address,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    xUserPublicKey: Option[PublicKey]
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(appendFilters(uri"$apiUri/matcher/orders/${owner.stringRepr}", activeOnly, closedOnly))
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  def getOrderHistoryByAssetPairAndPKWithSig(
    publicKey: String,
    amountAsset: String,
    priceAsset: String,
    timestamp: Long,
    signature: String
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/publicKey/$publicKey")
      .headers(Map("timestamp" -> timestamp.toString, "signature" -> signature))
  }

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  override def getOrderHistoryByAssetPairAndPKWithSig(
    owner: KeyPair,
    assetPair: AssetPair,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    timestamp: Long
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    basicRequest
      .get(
        appendFilters(
          uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/publicKey/${Base58.encode(owner.publicKey)}",
          activeOnly,
          closedOnly
        )
      )
      .headers(timestampAndSignatureHeaders(owner, timestamp))
  }

  override def getOrderBooks: R[HttpTradingMarkets] = mk(basicRequest.get(uri"$apiUri/matcher/orderbook"))

  override def getOrderBook(amountAsset: String, priceAsset: String): R[HttpV0OrderBook] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset")
      .followRedirects(false)
  }

  override def getOrderBook(assetPair: AssetPair): R[HttpV0OrderBook] = getOrderBook(assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def getOrderBook(assetPair: AssetPair, depth: String): R[HttpV0OrderBook] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}?depth=$depth")
      .followRedirects(true)
  }

  override def getOrderBook(assetPair: AssetPair, depth: Int): R[HttpV0OrderBook] = getOrderBook(assetPair, depth.toString)

  override def getOrderBookRestrictions(amountAsset: String, priceAsset: String): R[HttpOrderBookInfo] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/info")
      .followRedirects(false)
  }

  override def getOrderBookRestrictions(assetPair: AssetPair): R[HttpOrderBookInfo] =
    getOrderBookRestrictions(assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def getOrderBookStatus(amountAsset: String, priceAsset: String): R[HttpOrderBookStatus] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/status")
      .followRedirects(false)
      .headers(apiKeyHeaders)
  }

  override def getOrderBookStatus(assetPair: AssetPair): R[HttpOrderBookStatus] =
    getOrderBookStatus(assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def upsertAssetRate(assetId: String, rate: Double, headers: Map[String, String]): R[HttpMessage] = mk {
    basicRequest
      .put(uri"$apiUri/matcher/settings/rates/$assetId")
      .body(Json.stringify(Json.toJson(rate)))
      .contentType(MediaType.ApplicationJson)
      .headers(headers)
  }

  override def upsertAssetRate(asset: Asset, rate: String): R[HttpMessage] = mk {
    basicRequest
      .put(uri"$apiUri/matcher/settings/rates/${asset.toString}")
      .body(rate)
      .contentType(MediaType.ApplicationJson)
      .headers(apiKeyHeaders)
  }

  override def upsertAssetRate(asset: Asset, rate: Double): R[HttpMessage] = upsertAssetRate(asset.toString, rate, apiKeyHeaders)

  override def deleteAssetRate(assetId: String, headers: Map[String, String]): R[HttpMessage] = mk {
    basicRequest
      .delete(uri"$apiUri/matcher/settings/rates/$assetId")
      .contentType(MediaType.ApplicationJson)
      .headers(headers)
  }

  override def deleteAssetRate(asset: Asset): R[HttpMessage] = deleteAssetRate(asset.toString, apiKeyHeaders)

  override def getAssetRates: R[HttpRates] = mk {
    basicRequest.get(uri"$apiUri/matcher/settings/rates").headers(apiKeyHeaders)
  }

  override def deleteOrderBookWithKey(assetPair: AssetPair): R[HttpMessage] =
    deleteOrderBookWithKey(assetPair.amountAssetStr, assetPair.priceAssetStr, apiKeyHeaders)

  override def deleteOrderBookWithKey(amountAsset: String, priceAsset: String, headers: Map[String, String]): R[HttpMessage] = mk {
    basicRequest
      .delete(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset")
      .followRedirects(false)
      .headers(headers)
  }

  override def getCurrentOffset(headers: Map[String, String]): R[HttpOffset] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/debug/currentOffset")
      .headers(headers)
  }

  override def getCurrentOffset: R[HttpOffset] = getCurrentOffset(apiKeyHeaders)

  override def getLastOffset(headers: Map[String, String]): R[HttpOffset] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/debug/lastOffset")
      .headers(headers)
  }

  override def getLastOffset: R[HttpOffset] = getLastOffset(apiKeyHeaders)

  override def getOldestSnapshotOffset(headers: Map[String, String]): R[HttpOffset] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/debug/oldestSnapshotOffset")
      .headers(headers)
  }

  override def getOldestSnapshotOffset: R[HttpOffset] = getOldestSnapshotOffset(apiKeyHeaders)

  override def getAllSnapshotOffsets(headers: Map[String, String]): R[HttpSnapshotOffsets] = mk {
    basicRequest.get(uri"$apiUri/matcher/debug/allSnapshotOffsets").headers(headers)
  }

  override def getAllSnapshotOffsets: R[HttpSnapshotOffsets] = getAllSnapshotOffsets(apiKeyHeaders)

  override def saveSnapshots(headers: Map[String, String]): R[HttpMessage] = mk {
    basicRequest.post(uri"$apiUri/matcher/debug/saveSnapshots").headers(headers)
  }

  override def saveSnapshots: R[HttpMessage] = saveSnapshots(apiKeyHeaders)

  override def getMatcherPublicSettings: R[HttpMatcherPublicSettings] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/settings")
      .headers(apiKeyHeaders)
  }

  override def getMatcherConfig(headers: Map[String, String]): R[Config] = mkHocon {
    basicRequest
      .get(uri"$apiUri/matcher/debug/config")
      .headers(headers)
  }

  override def getMatcherConfig: R[Config] = getMatcherConfig(apiKeyHeaders)

  override def getAddressState(address: String, headers: Map[String, String]): R[HttpAddressState] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/debug/address/$address")
      .headers(headers)
  }

  override def getAddressState(address: Address): R[HttpAddressState] = getAddressState(address.stringRepr, apiKeyHeaders)

  override def getAddressState(address: Address, headers: Map[String, String]): R[HttpAddressState] = getAddressState(address.stringRepr, headers)

  override def getAddressState(address: String): R[HttpAddressState] = getAddressState(address, apiKeyHeaders)

  override def getMatcherStatus(headers: Map[String, String]): R[HttpSystemStatus] = mk {
    basicRequest
      .get(uri"$apiUri/matcher/debug/status")
      .headers(headers)
  }

  override def getMatcherStatus: R[HttpSystemStatus] = getMatcherStatus(apiKeyHeaders)

  override def getMatcherPKInBase58: R[String] = mk {
    basicRequest.get(uri"$apiUri/matcher")
  }

  override def printMessage(message: String): R[Unit] = mkIgnore {
    basicRequest
      .post(uri"$apiUri/matcher/debug/print")
      .headers(apiKeyHeaders)
      .body(Json.stringify(Json.toJson(HttpMessage(message))))
      .contentType(MediaType.ApplicationJson)
  }

  override def wsConnections: R[HttpWebSocketConnections] = mk {
    basicRequest
      .get(uri"$apiUri/ws/v0/connections")
      .headers(apiKeyHeaders)
  }

  override def closeWsConnections(oldestNumber: Int): R[HttpMessage] = mk {
    basicRequest
      .delete(uri"$apiUri/ws/v0/connections")
      .body(Json.toJson(HttpWebSocketCloseFilter(oldestNumber)).toString())
      .contentType(MediaType.ApplicationJson)
      .headers(apiKeyHeaders)
  }

  def cancelRequest(sender: KeyPair, orderId: String): HttpCancelOrder = {
    val req = HttpCancelOrder(sender, Some(ByteStr.decodeBase58(orderId).get), None, Array.emptyByteArray)
    val signature = crypto.sign(sender, req.toSign)
    req.copy(signature = signature)
  }

  def batchCancelRequest(sender: KeyPair, timestamp: Long): HttpCancelOrder = {
    val req = HttpCancelOrder(sender, None, Some(timestamp), Array.emptyByteArray)
    val signature = crypto.sign(sender, req.toSign)
    req.copy(signature = signature)
  }

  def appendFilters(uri: Uri, activeOnly: Option[Boolean], closedOnly: Option[Boolean]): Uri = {
    val activeOnlyQuery = boolQueryFragments("activeOnly", activeOnly)
    val closedOnlyQuery = boolQueryFragments("closedOnly", closedOnly)
    uri.copy(querySegments = activeOnlyQuery ++ closedOnlyQuery)
  }

  def boolQueryFragments(name: String, x: Option[Boolean]): List[QuerySegment] =
    x.fold(List.empty[QuerySegment])(x => List(QuerySegment.KeyValue(name, x.toString)))

  def timestampAndSignatureHeaders(owner: KeyPair, timestamp: Long): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> Base58.encode(crypto.sign(owner, owner.publicKey ++ Longs.toByteArray(timestamp)))
  )

  val apiKeyHeaders: Map[String, String] = Map("X-API-Key" -> apiKey)
  def userPublicKeyHeaders(x: PublicKey): Map[String, String] = Map("X-User-Public-Key" -> x.base58)

  def apiKeyWithUserPublicKeyHeaders(xUserPublicKey: Option[PublicKey]): Map[String, String] =
    apiKeyHeaders ++ xUserPublicKey.fold(Map.empty[String, String])(userPublicKeyHeaders)

}

object AsyncEnrichedDexApi {
  type R[EntityT] = Future[EnrichedResponse[MatcherError, EntityT]]
}
