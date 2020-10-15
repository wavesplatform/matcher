package com.wavesplatform.dex.it.dex

import java.net.InetSocketAddress
import java.util.UUID

import com.google.common.primitives.Longs
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
import com.wavesplatform.dex.it.dex.DexNewApi.AsyncEnriched
import com.wavesplatform.dex.it.json._
import im.mak.waves.transactions.ExchangeTransaction
import play.api.libs.json.{Json, Reads}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedDexNewApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing])
    extends DexNewApi[AsyncEnriched] {

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

  override def cancelWithApiKey(id: Id, xUserPublicKey: Option[PublicKey]): AsyncEnriched[HttpSuccessfulSingleCancel] = ???

  override def cancelAll(owner: KeyPair, timestamp: Long): AsyncEnriched[HttpSuccessfulBatchCancel] = ???

  override def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long): AsyncEnriched[HttpSuccessfulBatchCancel] = ???

  override def cancelAllByIdsWithApiKey(
    owner: Address,
    orderIds: Set[Id],
    xUserPublicKey: Option[PublicKey]
  ): AsyncEnriched[HttpSuccessfulBatchCancel] = ???

  override def orderStatus(assetPair: AssetPair, id: Id): AsyncEnriched[HttpOrderStatus] = ???

  override def orderStatusInfoByIdWithApiKey(
    owner: Address,
    orderId: Id,
    xUserPublicKey: Option[PublicKey]
  ): AsyncEnriched[HttpOrderBookHistoryItem] = ???

  override def orderStatusInfoByIdWithSignature(owner: KeyPair, orderId: Id, timestamp: Long): AsyncEnriched[HttpOrderBookHistoryItem] = ???

  override def transactionsByOrder(id: Id): AsyncEnriched[List[ExchangeTransaction]] = ???

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  override def orderHistory(
    owner: KeyPair,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    timestamp: Long
  ): AsyncEnriched[List[HttpOrderBookHistoryItem]] = ???

  /**
   * param @activeOnly Server treats this parameter as true if it wasn't specified
   */
  override def orderHistoryWithApiKey(
    owner: Address,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    xUserPublicKey: Option[PublicKey]
  ): AsyncEnriched[List[HttpOrderBookHistoryItem]] = ???

  /**
   * param @activeOnly Server treats this parameter as false if it wasn't specified
   */
  override def orderHistoryByPair(
    owner: KeyPair,
    assetPair: AssetPair,
    activeOnly: Option[Boolean],
    closedOnly: Option[Boolean],
    timestamp: Long
  ): AsyncEnriched[List[HttpOrderBookHistoryItem]] = ???

  override def allOrderBooks: AsyncEnriched[HttpTradingMarkets] = ???

  override def orderBook(assetPair: AssetPair): AsyncEnriched[HttpV0OrderBook] = ???

  override def orderBook(assetPair: AssetPair, depth: Int): AsyncEnriched[HttpV0OrderBook] = ???

  override def orderBookInfo(assetPair: AssetPair): AsyncEnriched[HttpOrderBookInfo] = ???

  override def orderBookStatus(assetPair: AssetPair): AsyncEnriched[HttpMarketStatus] = ???

  override def deleteOrderBook(assetPair: AssetPair): AsyncEnriched[HttpMessage] = ???

  override def upsertRate(asset: Asset, rate: Double): AsyncEnriched[HttpMessage] = ???

  override def deleteRate(asset: Asset): AsyncEnriched[HttpMessage] = ???

  override def rates: AsyncEnriched[HttpRates] = ???

  override def currentOffset: AsyncEnriched[HttpOffset] = ???

  override def lastOffset: AsyncEnriched[HttpOffset] = ???

  override def oldestSnapshotOffset: AsyncEnriched[HttpOffset] = ???

  override def allSnapshotOffsets: AsyncEnriched[HttpSnapshotOffsets] = ???

  override def saveSnapshots: AsyncEnriched[Unit] = ???

  override def settings: AsyncEnriched[HttpMatcherPublicSettings] = ???

  override def config: AsyncEnriched[Config] = ???

  override def wsConnections: AsyncEnriched[HttpWebSocketConnections] = ???

  override def closeWsConnections(oldestNumber: Int): AsyncEnriched[HttpMessage] = ???

  override def waitForOrder(assetPair: AssetPair, id: Id)(pred: HttpOrderStatus => Boolean): AsyncEnriched[HttpOrderStatus] = ???

  override def waitForOrderPlacement(order: Order): AsyncEnriched[HttpSuccessfulPlace] = ???

  override def waitForOrderHistory[A](
    owner: KeyPair,
    activeOnly: Option[Boolean]
  )(pred: List[HttpOrderBookHistoryItem] => Boolean): AsyncEnriched[List[HttpOrderBookHistoryItem]] = ???

  override def waitForOrderStatus(assetPair: AssetPair, id: Id, status: HttpOrderStatus.Status): AsyncEnriched[HttpOrderStatus] = ???

  override def waitForTransactionsByOrder(id: Id, atLeast: StatusCode): AsyncEnriched[List[ExchangeTransaction]] = ???

  override def waitForTransactionsByOrder(id: Id)(pred: List[ExchangeTransaction] => Boolean): AsyncEnriched[List[ExchangeTransaction]] = ???

  override def waitForCurrentOffset(pred: HttpOffset => Boolean): AsyncEnriched[HttpOffset] = ???

  override def waitForWsConnections(pred: HttpWebSocketConnections => Boolean): AsyncEnriched[HttpWebSocketConnections] = ???

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

  def mk[T: Reads](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map {
      EnrichedResponse[T](_)
    }

  def cancelRequest(sender: KeyPair, orderId: String): HttpCancelOrder = {
    val req = HttpCancelOrder(sender, Some(ByteStr.decodeBase58(orderId).get), None, Array.emptyByteArray)
    val signature = crypto.sign(sender, req.toSign)
    req.copy(signature = signature)
  }

  def timestampAndSignatureHeaders(owner: KeyPair, timestamp: Long): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> Base58.encode(crypto.sign(owner, owner.publicKey ++ Longs.toByteArray(timestamp)))
  )

  val apiKeyHeaders: Map[String, String] = Map("X-API-Key" -> apiKey)
  def userPublicKeyHeaders(x: PublicKey): Map[String, String] = Map("X-User-Public-Key" -> x.base58)

  def apiKeyWithUserPublicKeyHeaders(xUserPublicKey: Option[PublicKey]): Map[String, String] =
    apiKeyHeaders ++ xUserPublicKey.fold(Map.empty[String, String])(userPublicKeyHeaders)

}
