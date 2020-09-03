package com.wavesplatform.dex.it.dex

import cats.Functor
import cats.syntax.functor._
import com.softwaremill.sttp.StatusCode
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.fp.CanExtract
import com.wavesplatform.wavesj.transactions.ExchangeTransaction

object DexApiOps {
  // TODO replace by a macros
  final implicit class ExplicitGetDexApiOps[F[_]: CanExtract: Functor](val self: DexApi[F]) {

    private val canExtract: CanExtract[F] = implicitly[CanExtract[F]]; import canExtract.{extract => explicitGet}

    def publicKey: F[HttpMatcherPublicKey] = explicitGet(self.tryPublicKey)

    def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis()): F[HttpBalance] = {
      explicitGet(self.tryReservedBalance(of, timestamp))
    }

    def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey] = None): F[HttpBalance] = {
      explicitGet(self.tryReservedBalanceWithApiKey(of, xUserPublicKey))
    }

    def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[HttpBalance] = {
      explicitGet(self.tryTradableBalance(of, assetPair, timestamp))
    }

    def place(order: Order): F[HttpSuccessfulPlace]       = explicitGet(self.tryPlace(order))
    def placeMarket(order: Order): F[HttpSuccessfulPlace] = explicitGet(self.tryPlaceMarket(order))

    def cancel(owner: KeyPair, order: Order): F[HttpSuccessfulSingleCancel]                       = cancel(owner, order.assetPair, order.id())
    def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[HttpSuccessfulSingleCancel] = explicitGet(self.tryCancel(owner, assetPair, id))

    def cancelWithApiKey(order: Order, xUserPublicKey: Option[PublicKey] = None): F[HttpSuccessfulSingleCancel] =
      cancelWithApiKey(order.id(), xUserPublicKey)

    def cancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[HttpSuccessfulSingleCancel] =
      explicitGet(self.tryCancelWithApiKey(id, xUserPublicKey))

    def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis()): F[HttpSuccessfulBatchCancel] =
      explicitGet(self.tryCancelAll(owner, timestamp))

    def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[HttpSuccessfulBatchCancel] = {
      explicitGet(self.tryCancelAllByPair(owner, assetPair, timestamp))
    }

    def cancelAllByIdsWithApiKey(owner: Address, orderIds: Set[Order.Id], xUserPublicKey: Option[PublicKey] = None): F[HttpSuccessfulBatchCancel] =
      explicitGet(self.tryCancelAllByIdsWithApiKey(owner, orderIds, xUserPublicKey))

    def orderStatus(order: Order): F[HttpOrderStatus]                       = orderStatus(order.assetPair, order.id())
    def orderStatus(assetPair: AssetPair, id: Order.Id): F[HttpOrderStatus] = explicitGet(self.tryOrderStatus(assetPair, id))

    def transactionsByOrder(order: Order): F[List[ExchangeTransaction]] = transactionsByOrder(order.id())
    def transactionsByOrder(id: Order.Id): F[List[ExchangeTransaction]] = explicitGet(self.tryTransactionsByOrder(id))

    def orderHistory(owner: KeyPair,
                     activeOnly: Option[Boolean] = None,
                     closedOnly: Option[Boolean] = None,
                     timestamp: Long = System.currentTimeMillis()): F[List[HttpOrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistory(owner, activeOnly, closedOnly, timestamp))

    def orderHistoryWithApiKey(owner: Address,
                               activeOnly: Option[Boolean] = None,
                               closedOnly: Option[Boolean] = None,
                               xUserPublicKey: Option[PublicKey] = None): F[List[HttpOrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistoryWithApiKey(owner, activeOnly, closedOnly, xUserPublicKey))

    def orderHistoryByPair(owner: KeyPair,
                           assetPair: AssetPair,
                           activeOnly: Option[Boolean] = None,
                           closedOnly: Option[Boolean] = None,
                           timestamp: Long = System.currentTimeMillis()): F[List[HttpOrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistoryByPair(owner, assetPair, activeOnly, closedOnly, timestamp))

    def allOrderBooks: F[HttpTradingMarkets] = explicitGet(self.tryAllOrderBooks)

    def tradingPairInfo(assetPair: AssetPair): F[Option[HttpMarketDataWithMeta]] = allOrderBooks.map {
      _.markets.find(marketData => marketData.amountAsset == assetPair.amountAsset && marketData.priceAsset == assetPair.priceAsset)
    }

    def orderBook(assetPair: AssetPair): F[HttpV0OrderBook]             = explicitGet(self.tryOrderBook(assetPair))
    def orderBook(assetPair: AssetPair, depth: Int): F[HttpV0OrderBook] = explicitGet(self.tryOrderBook(assetPair, depth))

    def orderBookInfo(assetPair: AssetPair): F[HttpOrderBookInfo]  = explicitGet(self.tryOrderBookInfo(assetPair))
    def orderBookStatus(assetPair: AssetPair): F[HttpMarketStatus] = explicitGet(self.tryOrderBookStatus(assetPair))

    def upsertRate(asset: Asset, rate: Double): F[(StatusCode, HttpMessage)] = explicitGet(self.tryUpsertRate(asset, rate))
    def deleteRate(asset: Asset): F[HttpMessage]                             = explicitGet(self.tryDeleteRate(asset))
    def rates: F[HttpRates]                                                  = explicitGet(self.tryRates)

    def currentOffset: F[HttpOffset]               = explicitGet(self.tryCurrentOffset)
    def lastOffset: F[HttpOffset]                  = explicitGet(self.tryLastOffset)
    def oldestSnapshotOffset: F[HttpOffset]        = explicitGet(self.tryOldestSnapshotOffset)
    def allSnapshotOffsets: F[HttpSnapshotOffsets] = explicitGet(self.tryAllSnapshotOffsets)
    def saveSnapshots: F[Unit]                     = explicitGet(self.trySaveSnapshots)

    def settings: F[HttpMatcherPublicSettings] = explicitGet(self.trySettings)

    def wsConnections: F[HttpWebSocketConnections]         = explicitGet(self.tryWsConnections)
    def closeWsConnections(oldestNumber: Int): F[HttpMessage] = explicitGet(self.tryCloseWsConnections(oldestNumber))
  }
}
