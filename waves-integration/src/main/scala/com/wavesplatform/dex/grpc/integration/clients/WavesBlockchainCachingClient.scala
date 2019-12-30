package com.wavesplatform.dex.grpc.integration.clients

import java.time.Duration

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.caches.{AssetDescriptionsCache, BalancesCache, FeaturesCache}
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.{Observable, Observer}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainCachingClient(underlying: WavesBlockchainClient[Future], defaultCacheExpiration: FiniteDuration)(
    implicit monixScheduler: Scheduler,
    grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient[Future]
    with ScorexLogging {

  log.info("1")

  private val cacheExpiration: Duration = Duration.ofMillis(defaultCacheExpiration.toMillis)

  private val balancesCache          = new BalancesCache(underlying.spendableBalance)
  private val featuresCache          = new FeaturesCache(underlying.isFeatureActivated, invalidationPredicate = !_) // we don't keep knowledge about unactivated features
  private val assetDescriptionsCache = new AssetDescriptionsCache(underlying.assetDescription, cacheExpiration)

  override lazy val spendableBalanceChanges: Observable[SpendableBalanceChanges] = {
    // Updates balances cache by balances stream
    underlying.spendableBalanceChanges.subscribe {
      new Observer[SpendableBalanceChanges] {
        def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balancesCache.batchPut(elem); Continue }
        def onComplete(): Unit                                 = log.info("Balance changes stream completed!")
        def onError(ex: Throwable): Unit                       = ()
      }
    }
    underlying.spendableBalanceChanges
  }

  log.info("2")

  override def spendableBalance(address: Address, asset: Asset): Future[Long]                           = balancesCache.get(address -> asset).map(_.toLong)
  override def isFeatureActivated(id: Short): Future[Boolean]                                           = featuresCache.get(id) map Boolean2boolean
  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]]        = assetDescriptionsCache.get(asset)
  override def hasScript(asset: Asset.IssuedAsset): Future[Boolean]                                     = underlying.hasScript(asset)
  override def runScript(asset: Asset.IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)
  override def hasScript(address: Address): Future[Boolean]                                             = underlying.hasScript(address)
  override def runScript(address: Address, input: Order): Future[RunScriptResult]                       = underlying.runScript(address, input)
  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]                            = underlying.wereForged(txIds)
  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean]                                    = underlying.broadcastTx(tx)
  override def forgedOrder(orderId: ByteStr): Future[Boolean]                                           = underlying.forgedOrder(orderId)
}
