package com.wavesplatform.dex.grpc.integration.clients.async

import java.time.Duration

import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.caches.{AssetDescriptionsCache, BalancesCache1, FeaturesCache}
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer

import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainCachingClient(channel: ManagedChannel, monixScheduler: Scheduler)(implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainGrpcAsyncClient(channel, monixScheduler)(grpcExecutionContext)
    with ScorexLogging {

  private val defaultCacheExpiration: Duration = Duration.ofMinutes(1)
  private val noExpiration: Duration           = Duration.ofDays(30)

  private val balancesCache          = new BalancesCache1(super.spendableBalance, noExpiration)
  private val featuresCache          = new FeaturesCache(super.isFeatureActivated, defaultCacheExpiration)
  private val assetDescriptionsCache = new AssetDescriptionsCache(super.assetDescription, defaultCacheExpiration)

  /** Updates balances cache by balances stream */
  super.spendableBalanceChanges.subscribe {
    new Observer[SpendableBalanceChanges] {
      override def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balancesCache.batchPut(elem); Continue }
      override def onComplete(): Unit                                 = log.info("Balance changes stream completed!")
      override def onError(ex: Throwable): Unit                       = log.warn(s"Error while listening to the balance changes stream occurred: ${ex.getMessage}")
    }
  }(monixScheduler)

  override def spendableBalance(address: Address, asset: Asset): Future[Long]                    = balancesCache.get(address -> asset).map(_.toLong)
  override def isFeatureActivated(id: Short): Future[Boolean]                                    = featuresCache.get(id) map Boolean2boolean
  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = assetDescriptionsCache.get(asset)
}
