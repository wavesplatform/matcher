package com.wavesplatform.dex.grpc.integration.clients.async

import java.time.Duration

import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.caches.{AssetDescriptionsCache, BalancesCache, FeaturesCache}
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainCachingClient(channel: ManagedChannel, defaultCacheExpiration: FiniteDuration, monixScheduler: Scheduler)(
    implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainGrpcAsyncClient(channel, monixScheduler)(grpcExecutionContext)
    with ScorexLogging {

  private val cacheExpiration: Duration = Duration.ofMillis(defaultCacheExpiration.toMillis)

  private val nonExpiringBalancesCache       = new BalancesCache(super.spendableBalance)
  private val expiringFeaturesCache          = new FeaturesCache(super.isFeatureActivated, cacheExpiration)
  private val expiringAssetDescriptionsCache = new AssetDescriptionsCache(super.assetDescription, cacheExpiration)

  /** Updates balances cache by balances stream */
  super.spendableBalanceChanges.subscribe {
    new Observer[SpendableBalanceChanges] {
      def onNext(elem: SpendableBalanceChanges): Future[Ack] = { nonExpiringBalancesCache.batchPut(elem); Continue }
      def onComplete(): Unit                                 = log.info("Balance changes stream completed!")
      def onError(ex: Throwable): Unit                       = log.warn(s"Error while listening to the balance changes stream occurred: ${ex.getMessage}")
    }
  }(monixScheduler)

  override def spendableBalance(address: Address, asset: Asset): Future[Long]                    = nonExpiringBalancesCache.get(address -> asset).map(_.toLong)
  override def isFeatureActivated(id: Short): Future[Boolean]                                    = expiringFeaturesCache.get(id) map Boolean2boolean
  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = expiringAssetDescriptionsCache.get(asset)

  override def assetDecimals(asset: Asset.IssuedAsset): Future[Option[Int]] = assetDescription(asset).map { _.map(_.decimals) }
  override def assetDecimals(asset: Asset): Future[Option[Int]] = asset.fold { Future.successful(Option(8)) } { issuedAsset =>
    this.assetDescription(issuedAsset).map { _.map(_.decimals) }
  }
}
