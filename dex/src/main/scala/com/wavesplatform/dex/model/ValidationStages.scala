package com.wavesplatform.dex.model

import cats.data.EitherT
import cats.instances.future._
import com.wavesplatform.dex.actors.OrderBookAskAdapter
import com.wavesplatform.dex.actors.orderbook.OrderBookActor
import com.wavesplatform.dex.actors.orderbook.OrderBookActor.MarketStatus
import com.wavesplatform.dex.caches.{MatchingRulesCache, OrderFeeSettingsCache, RateCache}
import com.wavesplatform.dex.db.AssetsStorage
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.effect.{liftFutureAsync, liftValueAsync, FutureResult}
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.OrderValidator.AsyncBlockchain
import com.wavesplatform.dex.settings.{MatcherSettings, OrderFeeSettings}
import com.wavesplatform.dex.time.Time

import scala.concurrent.{ExecutionContext, Future}

object ValidationStages {

  def mkFirst(
    settings: MatcherSettings,
    matcherPublicKey: PublicKey,
    orderFeeSettingsCache: OrderFeeSettingsCache,
    matchingRulesCache: MatchingRulesCache,
    rateCache: RateCache,
    assetsCache: AssetsStorage,
    blockchain: AsyncBlockchain,
    transactionCreator: ExchangeTransactionCreator,
    time: Time,
    orderBookAskAdapter: OrderBookAskAdapter,
    lastProcessedOffset: => Long,
    blacklistedAddresses: Set[Address],
    hasMatcherAccountScript: Boolean
  )(o: Order)(implicit efc: ErrorFormatterContext, ec: ExecutionContext): FutureResult[Order] = {
    import OrderValidator._

    def actualOrderFeeSettings: OrderFeeSettings = orderFeeSettingsCache.getSettingsForOffset(lastProcessedOffset + 1)

    /** Does not need additional access to the blockchain via gRPC */
    def syncValidation(marketStatus: Option[MarketStatus], orderAssetsDecimals: Asset => Int): Either[MatcherError, Order] = {

      lazy val actualTickSize = matchingRulesCache
        .getNormalizedRuleForNextOrder(o.assetPair, lastProcessedOffset, orderAssetsDecimals)
        .tickSize

      for {
        _ <- matcherSettingsAware(matcherPublicKey, blacklistedAddresses, settings, orderAssetsDecimals, rateCache, actualOrderFeeSettings)(o)
        _ <- timeAware(time)(o)
        _ <- tickSizeAware(actualTickSize)(o)
        _ <-
          if (settings.maxPriceDeviations.enable) marketAware(actualOrderFeeSettings, settings.maxPriceDeviations, marketStatus)(o) else success
      } yield o
    }

    /** Needs additional asynchronous access to the blockchain */
    def asyncValidation(orderAssetsDescriptions: Asset => BriefAssetDescription): FutureResult[Order] =
      blockchainAware(
        blockchain,
        transactionCreator.createTransaction,
        time,
        actualOrderFeeSettings,
        settings.orderRestrictions.get(o.assetPair),
        orderAssetsDescriptions,
        rateCache,
        hasMatcherAccountScript
      )(o)

    for {
      marketStatus <- {
        if (settings.maxPriceDeviations.enable) EitherT(orderBookAskAdapter.getMarketStatus(o.assetPair))
        else liftValueAsync(Option.empty[OrderBookActor.MarketStatus])
      }
      _ <- liftAsync(syncValidation(marketStatus, assetsCache.unsafeGetDecimals))
      _ <- asyncValidation(assetsCache.unsafeGet)
    } yield o
  }

  def mkSecond(
    blockchain: AsyncBlockchain,
    orderBookAskAdapter: OrderBookAskAdapter
  )(ao: AcceptedOrder, tradableBalance: Map[Asset, Long])(implicit
    efc: ErrorFormatterContext,
    ec: ExecutionContext
  ): Future[Either[MatcherError, Unit]] = {
    for {
      hasOrderInBlockchain <- liftFutureAsync(blockchain.forgedOrder(ao.id))
      orderBookCache <- EitherT(orderBookAskAdapter.getAggregatedSnapshot(ao.order.assetPair))
      _ <- EitherT.fromEither {
        OrderValidator
          .accountStateAware(
            tradableBalance.withDefaultValue(0L),
            hasOrderInBlockchain,
            orderBookCache.getOrElse(OrderBookAggregatedSnapshot.empty)
          )(ao)
      }
    } yield ()
  }.value

}
