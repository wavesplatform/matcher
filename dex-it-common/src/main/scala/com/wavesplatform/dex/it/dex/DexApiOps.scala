package com.wavesplatform.dex.it.dex

import cats.Functor
import cats.syntax.functor._
import com.softwaremill.sttp.StatusCode
import com.wavesplatform.dex.api.{MatcherResponse => _, _}
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.fp.CanExtract
import com.wavesplatform.wavesj.transactions.ExchangeTransaction

object DexApiOps {
  // TODO replace by a macros
  final implicit class ExplicitGetDexApiOps[F[_]: CanExtract: Functor](val self: DexApi[F]) {

    private val canExtract: CanExtract[F] = implicitly[CanExtract[F]]; import canExtract.{extract => explicitGet}

    def publicKey: F[ApiMatcherPublicKey] = explicitGet(self.tryPublicKey)

    def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis()): F[ApiBalance] = {
      explicitGet(self.tryReservedBalance(of, timestamp))
    }

    def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey] = None): F[ApiBalance] = {
      explicitGet(self.tryReservedBalanceWithApiKey(of, xUserPublicKey))
    }

    def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[ApiBalance] = {
      explicitGet(self.tryTradableBalance(of, assetPair, timestamp))
    }

    def place(order: Order): F[ApiSuccessfulPlace]       = explicitGet(self.tryPlace(order))
    def placeMarket(order: Order): F[ApiSuccessfulPlace] = explicitGet(self.tryPlaceMarket(order))

    def cancel(owner: KeyPair, order: Order): F[ApiSuccessfulSingleCancel]                       = cancel(owner, order.assetPair, order.id())
    def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[ApiSuccessfulSingleCancel] = explicitGet(self.tryCancel(owner, assetPair, id))

    def cancelWithApiKey(order: Order, xUserPublicKey: Option[PublicKey] = None): F[ApiSuccessfulSingleCancel] =
      cancelWithApiKey(order.id(), xUserPublicKey)

    def cancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[ApiSuccessfulSingleCancel] =
      explicitGet(self.tryCancelWithApiKey(id, xUserPublicKey))

    def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis()): F[ApiSuccessfulBatchCancel] =
      explicitGet(self.tryCancelAll(owner, timestamp))

    def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[ApiSuccessfulBatchCancel] = {
      explicitGet(self.tryCancelAllByPair(owner, assetPair, timestamp))
    }

    def cancelAllByIdsWithApiKey(owner: Address, orderIds: Set[Order.Id], xUserPublicKey: Option[PublicKey] = None): F[ApiSuccessfulBatchCancel] =
      explicitGet(self.tryCancelAllByIdsWithApiKey(owner, orderIds, xUserPublicKey))

    def orderStatus(order: Order): F[ApiOrderStatus]                       = orderStatus(order.assetPair, order.id())
    def orderStatus(assetPair: AssetPair, id: Order.Id): F[ApiOrderStatus] = explicitGet(self.tryOrderStatus(assetPair, id))

    def transactionsByOrder(order: Order): F[List[ExchangeTransaction]] = transactionsByOrder(order.id())
    def transactionsByOrder(id: Order.Id): F[List[ExchangeTransaction]] = explicitGet(self.tryTransactionsByOrder(id))

    def orderHistory(owner: KeyPair,
                     activeOnly: Option[Boolean] = None,
                     closedOnly: Option[Boolean] = None,
                     timestamp: Long = System.currentTimeMillis()): F[List[ApiOrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistory(owner, activeOnly, closedOnly, timestamp))

    def orderHistoryWithApiKey(owner: Address,
                               activeOnly: Option[Boolean] = None,
                               closedOnly: Option[Boolean] = None,
                               xUserPublicKey: Option[PublicKey] = None): F[List[ApiOrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistoryWithApiKey(owner, activeOnly, closedOnly, xUserPublicKey))

    def orderHistoryByPair(owner: KeyPair,
                           assetPair: AssetPair,
                           activeOnly: Option[Boolean] = None,
                           closedOnly: Option[Boolean] = None,
                           timestamp: Long = System.currentTimeMillis()): F[List[ApiOrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistoryByPair(owner, assetPair, activeOnly, closedOnly, timestamp))

    def allOrderBooks: F[ApiTradingMarkets] = explicitGet(self.tryAllOrderBooks)

    def tradingPairInfo(assetPair: AssetPair): F[Option[ApiMarketDataWithMeta]] = allOrderBooks.map {
      _.markets.find(marketData => marketData.amountAsset == assetPair.amountAsset && marketData.priceAsset == assetPair.priceAsset)
    }

    def orderBook(assetPair: AssetPair): F[ApiV0OrderBook]             = explicitGet(self.tryOrderBook(assetPair))
    def orderBook(assetPair: AssetPair, depth: Int): F[ApiV0OrderBook] = explicitGet(self.tryOrderBook(assetPair, depth))

    def orderBookInfo(assetPair: AssetPair): F[ApiOrderBookInfo]  = explicitGet(self.tryOrderBookInfo(assetPair))
    def orderBookStatus(assetPair: AssetPair): F[ApiMarketStatus] = explicitGet(self.tryOrderBookStatus(assetPair))

    def upsertRate(asset: Asset, rate: Double): F[(StatusCode, ApiMessage)] = explicitGet(self.tryUpsertRate(asset, rate))
    def deleteRate(asset: Asset): F[ApiMessage]                             = explicitGet(self.tryDeleteRate(asset))
    def rates: F[ApiRates]                                                  = explicitGet(self.tryRates)

    def currentOffset: F[ApiOffset]               = explicitGet(self.tryCurrentOffset)
    def lastOffset: F[ApiOffset]                  = explicitGet(self.tryLastOffset)
    def oldestSnapshotOffset: F[ApiOffset]        = explicitGet(self.tryOldestSnapshotOffset)
    def allSnapshotOffsets: F[ApiSnapshotOffsets] = explicitGet(self.tryAllSnapshotOffsets)
    def saveSnapshots: F[Unit]                    = explicitGet(self.trySaveSnapshots)

    def settings: F[ApiMatcherPublicSettings] = explicitGet(self.trySettings)
  }
}
