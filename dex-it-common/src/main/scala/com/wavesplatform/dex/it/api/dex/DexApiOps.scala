package com.wavesplatform.dex.it.api.dex

import cats.Functor
import cats.syntax.functor._
import com.softwaremill.sttp.StatusCode
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.api.responses.dex._
import com.wavesplatform.dex.it.fp.CanExtract
import com.wavesplatform.wavesj.transactions.ExchangeTransaction

object DexApiOps {
  // TODO replace by a macros
  final implicit class ExplicitGetDexApiOps[F[_]: CanExtract: Functor](val self: DexApi[F]) {

    private val canExtract: CanExtract[F] = implicitly[CanExtract[F]]; import canExtract.{extract => explicitGet}

    def publicKey: F[PublicKey] = explicitGet(self.tryPublicKey)

    def reservedBalance(of: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Map[Asset, Long]] = {
      explicitGet(self.tryReservedBalance(of, timestamp))
    }

    def reservedBalanceWithApiKey(of: KeyPair, xUserPublicKey: Option[PublicKey] = None): F[Map[Asset, Long]] = {
      explicitGet(self.tryReservedBalanceWithApiKey(of, xUserPublicKey))
    }

    def tradableBalance(of: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Map[Asset, Long]] = {
      explicitGet(self.tryTradableBalance(of, assetPair, timestamp))
    }

    def place(order: Order): F[MatcherResponse]       = explicitGet(self.tryPlace(order))
    def placeMarket(order: Order): F[MatcherResponse] = explicitGet(self.tryPlaceMarket(order))

    def cancel(owner: KeyPair, order: Order): F[MatcherStatusResponse]                       = cancel(owner, order.assetPair, order.id())
    def cancel(owner: KeyPair, assetPair: AssetPair, id: Order.Id): F[MatcherStatusResponse] = explicitGet(self.tryCancel(owner, assetPair, id))

    def cancelWithApiKey(order: Order, xUserPublicKey: Option[PublicKey] = None): F[MatcherStatusResponse] =
      cancelWithApiKey(order.id(), xUserPublicKey)

    def cancelWithApiKey(id: Order.Id, xUserPublicKey: Option[PublicKey]): F[MatcherStatusResponse] =
      explicitGet(self.tryCancelWithApiKey(id, xUserPublicKey))

    def cancelAll(owner: KeyPair, timestamp: Long = System.currentTimeMillis()): F[Unit] = explicitGet(self.tryCancelAll(owner, timestamp))
    def cancelAllByPair(owner: KeyPair, assetPair: AssetPair, timestamp: Long = System.currentTimeMillis()): F[Unit] = {
      explicitGet(self.tryCancelAllByPair(owner, assetPair, timestamp))
    }

    def cancelAllByIdsWithApiKey(owner: Address, orderIds: Set[Order.Id], xUserPublicKey: Option[PublicKey] = None): F[Unit] =
      explicitGet(self.tryCancelAllByIdsWithApiKey(owner, orderIds, xUserPublicKey))

    def orderStatus(order: Order): F[OrderStatusResponse]                       = orderStatus(order.assetPair, order.id())
    def orderStatus(assetPair: AssetPair, id: Order.Id): F[OrderStatusResponse] = explicitGet(self.tryOrderStatus(assetPair, id))

    def transactionsByOrder(order: Order): F[List[ExchangeTransaction]] = transactionsByOrder(order.id())
    def transactionsByOrder(id: Order.Id): F[List[ExchangeTransaction]] = explicitGet(self.tryTransactionsByOrder(id))

    def orderHistory(owner: KeyPair,
                     activeOnly: Option[Boolean] = None,
                     timestamp: Long = System.currentTimeMillis()): F[List[OrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistory(owner, activeOnly, timestamp))

    def orderHistoryWithApiKey(owner: Address,
                               activeOnly: Option[Boolean] = None,
                               xUserPublicKey: Option[PublicKey] = None): F[List[OrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistoryWithApiKey(owner, activeOnly, xUserPublicKey))

    def orderHistoryByPair(owner: KeyPair,
                           assetPair: AssetPair,
                           activeOnly: Option[Boolean] = None,
                           timestamp: Long = System.currentTimeMillis()): F[List[OrderBookHistoryItem]] =
      explicitGet(self.tryOrderHistoryByPair(owner, assetPair, activeOnly, timestamp))

    def allOrderBooks: F[MarketDataInfo] = explicitGet(self.tryAllOrderBooks)

    def tradingPairInfo(assetPair: AssetPair): F[Option[MarketData]] = allOrderBooks.map {
      _.markets.find(marketData => marketData.amountAsset == assetPair.amountAssetStr && marketData.priceAsset == assetPair.priceAssetStr)
    }

    def orderBook(assetPair: AssetPair): F[OrderBookResponse]             = explicitGet(self.tryOrderBook(assetPair))
    def orderBook(assetPair: AssetPair, depth: Int): F[OrderBookResponse] = explicitGet(self.tryOrderBook(assetPair, depth))

    def orderBookInfo(assetPair: AssetPair): F[OrderBookInfo]          = explicitGet(self.tryOrderBookInfo(assetPair))
    def orderBookStatus(assetPair: AssetPair): F[MarketStatusResponse] = explicitGet(self.tryOrderBookStatus(assetPair))

    def upsertRate(asset: Asset, rate: Double): F[(StatusCode, RatesResponse)] = explicitGet(self.tryUpsertRate(asset, rate))
    def deleteRate(asset: Asset): F[RatesResponse]                             = explicitGet(self.tryDeleteRate(asset))
    def rates: F[Map[Asset, Double]]                                           = explicitGet(self.tryRates)

    def currentOffset: F[Long]                      = explicitGet(self.tryCurrentOffset)
    def lastOffset: F[Long]                         = explicitGet(self.tryLastOffset)
    def oldestSnapshotOffset: F[Long]               = explicitGet(self.tryOldestSnapshotOffset)
    def allSnapshotOffsets: F[Map[AssetPair, Long]] = explicitGet(self.tryAllSnapshotOffsets)
    def saveSnapshots: F[Unit]                      = explicitGet(self.trySaveSnapshots)

    def settings: F[SettingsResponse] = explicitGet(self.trySettings)
  }
}
