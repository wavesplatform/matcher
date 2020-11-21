package com.wavesplatform.dex.it.api.dex

import java.net.InetSocketAddress
import java.util.UUID

import com.google.common.primitives.Longs
import com.softwaremill.sttp.Uri.QueryFragment
import com.softwaremill.sttp._
import com.softwaremill.sttp.playJson._
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
import play.api.libs.json.Json

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedDexApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing])
    extends AsyncEnrichedApi[MatcherError](host)
    with DexApi[AsyncEnrichedDexApi.R] {

  override val publicKey: R[HttpMatcherPublicKey] = mk(sttp.get(uri"$apiUri/matcher"))

  override def getReservedBalance(publicKey: String, timestamp: Long, signature: String): R[HttpBalance] =
    getReservedBalance(publicKey, Map("timestamp" -> timestamp.toString, "signature" -> signature))

  override def getReservedBalance(of: KeyPair, timestamp: Long): R[HttpBalance] =
    getReservedBalance(Base58.encode(of.publicKey), timestampAndSignatureHeaders(of, timestamp))

  override def getReservedBalance(publicKey: String, headers: Map[String, String]): R[HttpBalance] = mk {
    sttp
      .get(uri"$apiUri/matcher/balance/reserved/$publicKey")
      .headers(headers)
  }

  override def getReservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): R[HttpBalance] = mk {
    sttp
      .get(uri"$apiUri/matcher/balance/reserved/${Base58.encode(of.publicKey)}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  override def getTradableBalance(address: String, amountAsset: String, priceAsset: String): R[HttpBalance] = mk {
    sttp
      .get(
        uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/tradableBalance/$address"
      )
      .followRedirects(false)
  }

  override def getTradableBalance(of: KeyPair, assetPair: AssetPair): R[HttpBalance] =
    getTradableBalance(of.publicKey.toAddress.stringRepr, assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def place(order: Order): R[HttpSuccessfulPlace] = mk {
    sttp
      .post(uri"$apiUri/matcher/orderbook")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false) // TODO move ?
      .body(order)
  }

  override def placeMarket(order: Order): R[HttpSuccessfulPlace] = mk {
    sttp.post(uri"$apiUri/matcher/orderbook/market").body(order)
  }

  override def cancel(owner: KeyPair, assetPair: AssetPair, id: Id): R[HttpSuccessfulSingleCancel] = mk {
    val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
    sttp
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false)
      .body(body)
      .contentType("application/json", "UTF-8")
  }

  override def cancelWithApiKey(id: Id, xUserPublicKey: Option[PublicKey]): R[HttpSuccessfulSingleCancel] = mk {
    sttp
      .post(uri"$apiUri/matcher/orders/cancel/${id.toString}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
      .contentType("application/json", "UTF-8")
  }

  override def cancelAll(owner: KeyPair, timestamp: Long): R[HttpSuccessfulBatchCancel] = mk {
    val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
    sttp
      .post(uri"$apiUri/matcher/orderbook/cancel")
      .body(body)
      .contentType("application/json", "UTF-8")
  }

  override def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long): R[HttpSuccessfulBatchCancel] = mk {
    val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
    sttp
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
      .body(body)
      .contentType("application/json", "UTF-8")
  }

  override def cancelAllByIdsWithApiKey(
    owner: Address,
    orderIds: Set[Id],
    xUserPublicKey: Option[PublicKey] = None
  ): R[HttpSuccessfulBatchCancel] = mk {
    sttp
      .post(uri"$apiUri/matcher/orders/$owner/cancel")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
      .body(Json.stringify(Json.toJson(orderIds)))
      .contentType("application/json", "UTF-8")
  }

  override def getOrderStatus(amountAsset: String, priceAsset: String, id: String): R[HttpOrderStatus] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/$id")
      .readTimeout(3.minutes) // TODO find way to decrease timeout!
      .followRedirects(false)
  }

  override def getOrderStatusInfoByIdWithApiKey(
    owner: Address,
    orderId: Id,
    xUserPublicKey: Option[PublicKey]
  ): R[HttpOrderBookHistoryItem] = mk {
    sttp
      .get(uri"$apiUri/matcher/orders/$owner/${orderId.toString}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  override def getOrderStatusInfoByIdWithSignature(publicKey: String, orderId: String, headers: Map[String, String]): R[HttpOrderBookHistoryItem] =
    mk {
      sttp
        .get(uri"$apiUri/matcher/orderbook/$publicKey/$orderId")
        .headers(headers)
    }

  override def getOrderStatusInfoByIdWithSignature(
    publicKey: String,
    orderId: String,
    timestamp: Long,
    signature: String
  ): R[HttpOrderBookHistoryItem] =
    getOrderStatusInfoByIdWithSignature(publicKey, orderId, Map("timestamp" -> timestamp.toString, "signature" -> signature))

  override def getOrderStatusInfoByIdWithSignature(owner: KeyPair, orderId: Id, timestamp: Long): R[HttpOrderBookHistoryItem] =
    getOrderStatusInfoByIdWithSignature(owner.publicKey.toString, orderId.toString, timestampAndSignatureHeaders(owner, timestamp))

  override def transactionsByOrder(id: Id): R[List[ExchangeTransaction]] = mk {
    sttp.get(uri"$apiUri/matcher/transactions/$id")
  }

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  override def orderHistory(
    owner: KeyPair,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    timestamp: Long
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    sttp
      .get(appendFilters(uri"$apiUri/matcher/orderbook/${Base58.encode(owner.publicKey)}", activeOnly, closedOnly))
      .headers(timestampAndSignatureHeaders(owner, timestamp))
  }

  /**
   * param @activeOnly Server treats this parameter as true if it wasn't specified
   */
  override def orderHistoryWithApiKey(
    owner: Address,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    xUserPublicKey: Option[PublicKey]
  ): R[List[HttpOrderBookHistoryItem]] = mk {
    sttp
      .get(appendFilters(uri"$apiUri/matcher/orders/${owner.stringRepr}", activeOnly, closedOnly))
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  override def orderHistoryByPair(
    owner: KeyPair,
    assetPair: AssetPair,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    timestamp: Long
  ): R[List[HttpOrderBookHistoryItem]] = mk {
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

  override def getOrderBooks: R[HttpTradingMarkets] = mk(sttp.get(uri"$apiUri/matcher/orderbook"))

  override def getOrderBook(amountAsset: String, priceAsset: String): R[HttpV0OrderBook] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset")
      .followRedirects(false)
  }

  override def getOrderBook(assetPair: AssetPair): R[HttpV0OrderBook] = getOrderBook(assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def getOrderBook(assetPair: AssetPair, depth: String): R[HttpV0OrderBook] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}?depth=$depth")
      .followRedirects(true)
  }

  override def getOrderBook(assetPair: AssetPair, depth: Int): R[HttpV0OrderBook] = getOrderBook(assetPair, depth.toString)

  override def getOrderBookInfo(amountAsset: String, priceAsset: String): R[HttpOrderBookInfo] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/info")
      .followRedirects(false)
  }

  override def getOrderBookInfo(assetPair: AssetPair): R[HttpOrderBookInfo] = getOrderBookInfo(assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def getOrderBookStatus(amountAsset: String, priceAsset: String): R[HttpOrderBookStatus] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/$amountAsset/$priceAsset/status")
      .followRedirects(false)
      .headers(apiKeyHeaders)
  }

  override def getOrderBookStatus(assetPair: AssetPair): R[HttpOrderBookStatus] =
    getOrderBookStatus(assetPair.amountAssetStr, assetPair.priceAssetStr)

  override def deleteOrderBook(assetPair: AssetPair): R[HttpMessage] = mk {
    sttp
      .delete(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
      .followRedirects(false)
      .headers(apiKeyHeaders)
  }

  override def upsertRate(asset: Asset, rate: Double): R[HttpMessage] = mk {
    sttp
      .put(uri"$apiUri/matcher/settings/rates/${asset.toString}")
      .body(Json.stringify(Json.toJson(rate)))
      .contentType("application/json", "UTF-8")
      .headers(apiKeyHeaders)
      .tag("requestId", UUID.randomUUID)
  }

  override def deleteRate(asset: Asset): R[HttpMessage] = mk {
    sttp
      .delete(uri"$apiUri/matcher/settings/rates/${asset.toString}")
      .contentType("application/json", "UTF-8")
      .headers(apiKeyHeaders)
  }

  override def rates: R[HttpRates] = mk {
    sttp.get(uri"$apiUri/matcher/settings/rates").headers(apiKeyHeaders)
  }

  override def currentOffset: R[HttpOffset] = mk {
    sttp
      .get(uri"$apiUri/matcher/debug/currentOffset")
      .headers(apiKeyHeaders)
  }

  override def lastOffset: R[HttpOffset] = mk {
    sttp
      .get(uri"$apiUri/matcher/debug/lastOffset")
      .headers(apiKeyHeaders)
  }

  override def oldestSnapshotOffset: R[HttpOffset] = mk {
    sttp
      .get(uri"$apiUri/matcher/debug/oldestSnapshotOffset")
      .headers(apiKeyHeaders)
  }

  override def allSnapshotOffsets: R[HttpSnapshotOffsets] = mk {
    sttp.get(uri"$apiUri/matcher/debug/allSnapshotOffsets").headers(apiKeyHeaders)
  }

  override def saveSnapshots: R[HttpMessage] = mk {
    sttp.post(uri"$apiUri/matcher/debug/saveSnapshots").headers(apiKeyHeaders)
  }

  override def settings: R[HttpMatcherPublicSettings] = mk {
    sttp
      .get(uri"$apiUri/matcher/settings")
      .headers(apiKeyHeaders)
  }

  override def config: R[Config] = mkHocon {
    sttp
      .get(uri"$apiUri/matcher/debug/config")
      .headers(apiKeyHeaders)
  }

  override def wsConnections: R[HttpWebSocketConnections] = mk {
    sttp
      .get(uri"$apiUri/ws/v0/connections")
      .headers(apiKeyHeaders)
  }

  override def closeWsConnections(oldestNumber: Int): R[HttpMessage] = mk {
    sttp
      .delete(uri"$apiUri/ws/v0/connections")
      .body(Json.toJson(HttpWebSocketCloseFilter(oldestNumber)).toString())
      .contentType("application/json", "UTF-8")
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
    uri.copy(queryFragments = activeOnlyQuery ++ closedOnlyQuery)
  }

  def boolQueryFragments(name: String, x: Option[Boolean]): List[QueryFragment] =
    x.fold(List.empty[QueryFragment])(x => List(QueryFragment.KeyValue(name, x.toString)))

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
