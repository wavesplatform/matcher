package com.wavesplatform.dex.it.dex

import java.net.InetSocketAddress
import java.util.UUID

import com.softwaremill.sttp.{SttpBackend, _}
import com.typesafe.config.Config
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.it.api.EnrichedResponse
import com.wavesplatform.dex.it.dex.DexNewApi.AsyncEnriched
import im.mak.waves.transactions.ExchangeTransaction
import play.api.libs.json.Reads

import scala.concurrent.{ExecutionContext, Future}

class AsyncEnrichedDexNewApi(apiKey: String, host: => InetSocketAddress)(implicit ec: ExecutionContext, httpBackend: SttpBackend[Future, Nothing])
    extends DexNewApi[AsyncEnriched] {

  override val publicKey: AsyncEnriched[HttpMatcherPublicKey] = mk[HttpMatcherPublicKey](sttp.get(uri"$apiUri/matcher"))

  override def reservedBalance(of: KeyPair, timestamp: Long): AsyncEnriched[HttpBalance] = ???

  override def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey]): AsyncEnriched[HttpBalance] = ???

  override def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long): AsyncEnriched[HttpBalance] = ???

  override def place(order: Order): AsyncEnriched[HttpSuccessfulPlace] = ???

  override def placeMarket(order: Order): AsyncEnriched[HttpSuccessfulPlace] = ???

  override def cancel(owner: KeyPair, assetPair: AssetPair, id: Id): AsyncEnriched[HttpSuccessfulSingleCancel] = ???

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

  def apiUri: String = {
    val savedHost = host
    s"http://${savedHost.getAddress.getHostAddress}:${savedHost.getPort}"
  }

  def mk[T: Reads](req: Request[String, Nothing]): AsyncEnriched[T] =
    httpBackend.send[String](req.tag("requestId", UUID.randomUUID)).map {
      EnrichedResponse[T](_)
    }

}
