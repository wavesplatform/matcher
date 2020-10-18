package com.wavesplatform.dex.it.dex

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
import com.wavesplatform.dex.it.api.EnrichedResponse
import com.wavesplatform.dex.it.dex.DexApi.AsyncEnriched
import com.wavesplatform.dex.it.json._
import im.mak.waves.transactions.ExchangeTransaction
import play.api.libs.json.{Json, Reads}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedDexApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing])
    extends DexApi[AsyncEnriched] {

  override val publicKey: AsyncEnriched[HttpMatcherPublicKey] = mk(sttp.get(uri"$apiUri/matcher"))

  override def reservedBalance(of: KeyPair, timestamp: Long): AsyncEnriched[HttpBalance] = mk {
    sttp
      .get(uri"$apiUri/matcher/balance/reserved/${Base58.encode(of.publicKey)}")
      .headers(timestampAndSignatureHeaders(of, timestamp))
  }

  override def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): AsyncEnriched[HttpBalance] = mk {
    sttp
      .get(uri"$apiUri/matcher/balance/reserved/${Base58.encode(of.publicKey)}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  override def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long): AsyncEnriched[HttpBalance] = mk {
    sttp
      .get(
        uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/tradableBalance/${of.publicKey.toAddress.stringRepr}"
      )
      .headers(timestampAndSignatureHeaders(of, timestamp))
  }

  override def place(order: Order): AsyncEnriched[HttpSuccessfulPlace] = mk {
    sttp
      .post(uri"$apiUri/matcher/orderbook")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false) // TODO move ?
      .body(order)
  }

  override def placeMarket(order: Order): AsyncEnriched[HttpSuccessfulPlace] = mk {
    sttp.post(uri"$apiUri/matcher/orderbook/market").body(order)
  }

  override def cancel(owner: KeyPair, assetPair: AssetPair, id: Id): AsyncEnriched[HttpSuccessfulSingleCancel] = mk {
    val body = Json.stringify(Json.toJson(cancelRequest(owner, id.toString)))
    sttp
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
      .readTimeout(3.minutes) // TODO find a way to decrease the timeout!
      .followRedirects(false)
      .body(body)
      .contentType("application/json", "UTF-8")
  }

  override def cancelWithApiKey(id: Id, xUserPublicKey: Option[PublicKey]): AsyncEnriched[HttpSuccessfulSingleCancel] = mk {
    sttp
      .post(uri"$apiUri/matcher/orders/cancel/${id.toString}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
      .contentType("application/json", "UTF-8")
  }

  override def cancelAll(owner: KeyPair, timestamp: Long): AsyncEnriched[HttpSuccessfulBatchCancel] = mk {
    val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
    sttp
      .post(uri"$apiUri/matcher/orderbook/cancel")
      .body(body)
      .contentType("application/json", "UTF-8")
  }

  override def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long): AsyncEnriched[HttpSuccessfulBatchCancel] = mk {
    val body = Json.stringify(Json.toJson(batchCancelRequest(owner, timestamp)))
    sttp
      .post(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/cancel")
      .body(body)
      .contentType("application/json", "UTF-8")
  }

  override def cancelAllByIdsWithApiKey(
    owner: Address,
    orderIds: Set[Id],
    xUserPublicKey: Option[PublicKey]
  ): AsyncEnriched[HttpSuccessfulBatchCancel] = mk {
    sttp
      .post(uri"$apiUri/matcher/orders/$owner/cancel")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
      .body(Json.stringify(Json.toJson(orderIds)))
      .contentType("application/json", "UTF-8")
  }

  override def orderStatus(assetPair: AssetPair, id: Id): AsyncEnriched[HttpOrderStatus] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/$id")
      .readTimeout(3.minutes) // TODO find way to decrease timeout!
      .followRedirects(false)
  }

  override def orderStatusInfoByIdWithApiKey(
    owner: Address,
    orderId: Id,
    xUserPublicKey: Option[PublicKey]
  ): AsyncEnriched[HttpOrderBookHistoryItem] = mk {
    sttp
      .get(uri"$apiUri/matcher/orders/$owner/${orderId.toString}")
      .headers(apiKeyWithUserPublicKeyHeaders(xUserPublicKey))
  }

  override def orderStatusInfoByIdWithSignature(owner: KeyPair, orderId: Id, timestamp: Long): AsyncEnriched[HttpOrderBookHistoryItem] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${owner.publicKey}/${orderId.toString}")
      .headers(timestampAndSignatureHeaders(owner, timestamp))
  }

  override def transactionsByOrder(id: Id): AsyncEnriched[List[ExchangeTransaction]] = mk {
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
  ): AsyncEnriched[List[HttpOrderBookHistoryItem]] = mk {
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
  ): AsyncEnriched[List[HttpOrderBookHistoryItem]] = mk {
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
  ): AsyncEnriched[List[HttpOrderBookHistoryItem]] = mk {
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

  override def allOrderBooks: AsyncEnriched[HttpTradingMarkets] = mk(sttp.get(uri"$apiUri/matcher/orderbook"))

  override def orderBook(assetPair: AssetPair): AsyncEnriched[HttpV0OrderBook] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
      .followRedirects(false)
  }

  override def orderBook(assetPair: AssetPair, depth: Int): AsyncEnriched[HttpV0OrderBook] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}?depth=$depth")
      .followRedirects(true)
  }

  override def orderBookInfo(assetPair: AssetPair): AsyncEnriched[HttpOrderBookInfo] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/info")
      .followRedirects(false)
  }

  override def orderBookStatus(assetPair: AssetPair): AsyncEnriched[HttpMarketStatus] = mk {
    sttp
      .get(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}/status")
      .followRedirects(false)
      .headers(apiKeyHeaders)
  }

  override def deleteOrderBook(assetPair: AssetPair): AsyncEnriched[HttpMessage] = mk {
    sttp
      .delete(uri"$apiUri/matcher/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}")
      .followRedirects(false)
      .headers(apiKeyHeaders)
  }

  override def upsertRate(asset: Asset, rate: Double): AsyncEnriched[HttpMessage] = mk {
    sttp
      .put(uri"$apiUri/matcher/settings/rates/${asset.toString}")
      .body(Json.stringify(Json.toJson(rate)))
      .contentType("application/json", "UTF-8")
      .headers(apiKeyHeaders)
      .tag("requestId", UUID.randomUUID)
  }

  override def deleteRate(asset: Asset): AsyncEnriched[HttpMessage] = mk {
    sttp
      .delete(uri"$apiUri/matcher/settings/rates/${asset.toString}")
      .contentType("application/json", "UTF-8")
      .headers(apiKeyHeaders)
  }

  override def rates: AsyncEnriched[HttpRates] = mk {
    sttp.get(uri"$apiUri/matcher/settings/rates").headers(apiKeyHeaders)
  }

  override def currentOffset: AsyncEnriched[HttpOffset] = mk {
    sttp
      .get(uri"$apiUri/matcher/debug/currentOffset")
      .headers(apiKeyHeaders)
  }

  override def lastOffset: AsyncEnriched[HttpOffset] = mk {
    sttp
      .get(uri"$apiUri/matcher/debug/lastOffset")
      .headers(apiKeyHeaders)
  }

  override def oldestSnapshotOffset: AsyncEnriched[HttpOffset] = mk {
    sttp
      .get(uri"$apiUri/matcher/debug/oldestSnapshotOffset")
      .headers(apiKeyHeaders)
  }

  override def allSnapshotOffsets: AsyncEnriched[HttpSnapshotOffsets] = mk {
    sttp.get(uri"$apiUri/matcher/debug/allSnapshotOffsets").headers(apiKeyHeaders)
  }

  override def saveSnapshots: AsyncEnriched[HttpMessage] = mk {
    sttp.post(uri"$apiUri/matcher/debug/saveSnapshots").headers(apiKeyHeaders)
  }

  override def settings: AsyncEnriched[HttpMatcherPublicSettings] = mk {
    sttp
      .get(uri"$apiUri/matcher/settings")
      .headers(apiKeyHeaders)
  }

  override def config: AsyncEnriched[Config] = mkHocon {
    sttp
      .get(uri"$apiUri/matcher/debug/config")
      .headers(apiKeyHeaders)
  }

  override def wsConnections: AsyncEnriched[HttpWebSocketConnections] = mk {
    sttp
      .get(uri"$apiUri/ws/v0/connections")
      .headers(apiKeyHeaders)
  }

  override def closeWsConnections(oldestNumber: Int): AsyncEnriched[HttpMessage] = mk {
    sttp
      .delete(uri"$apiUri/ws/v0/connections")
      .body(Json.toJson(HttpWebSocketCloseFilter(oldestNumber)).toString())
      .contentType("application/json", "UTF-8")
      .headers(apiKeyHeaders)
  }

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

  def mk[T: Reads](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map {
      EnrichedResponse.AsJson[T](_)
    }

  def mkHocon[T](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map(EnrichedResponse.AsHocon[T])

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
