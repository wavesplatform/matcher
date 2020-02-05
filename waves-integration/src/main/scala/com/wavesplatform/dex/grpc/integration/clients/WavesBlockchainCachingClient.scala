package com.wavesplatform.dex.grpc.integration.clients

import java.time.Duration

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.caches.{AssetDescriptionsCache, BalancesCache, FeaturesCache}
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.{Observable, Observer}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class WavesBlockchainCachingClient(underlying: WavesBlockchainClient[Future], defaultCacheExpiration: FiniteDuration, monixScheduler: Scheduler)(
    implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient[Future]
    with ScorexLogging {

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
    }(monixScheduler)
    underlying.spendableBalanceChanges
  }

  override def spendableBalance(address: Address, asset: Asset): Future[Long] = balancesCache.get(address -> asset).map(_.toLong)

  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] = underlying.allAssetsSpendableBalance(address) andThen {
    case Success(bs) => balancesCache.batchPut(Map(address -> bs))
    case Failure(t)  => log.error("Cannot update balance cache!", t)
  }

  override def isFeatureActivated(id: Short): Future[Boolean]                                           = featuresCache.get(id) map Boolean2boolean
  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]]        = assetDescriptionsCache.get(asset)
  override def hasScript(asset: Asset.IssuedAsset): Future[Boolean]                                     = underlying.hasScript(asset)
  override def runScript(asset: Asset.IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)
  override def hasScript(address: Address): Future[Boolean]                                             = underlying.hasScript(address)
  override def runScript(address: Address, input: Order): Future[RunScriptResult]                       = underlying.runScript(address, input)
  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]                           = underlying.wereForged(txIds)
  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean]                                    = underlying.broadcastTx(tx)
  override def forgedOrder(orderId: ByteStr): Future[Boolean]                                           = underlying.forgedOrder(orderId)
  override def close(): Future[Unit]                                                                    = underlying.close()
}
