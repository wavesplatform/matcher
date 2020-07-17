package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress
import java.time.Duration

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.caches.{AssetDescriptionsCache, FeaturesCache}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.reactive.Observable

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainCachingClient(underlying: WavesBlockchainClient[Future], defaultCacheExpiration: FiniteDuration)(
    implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient[Future]
    with ScorexLogging {

  private val cacheExpiration: Duration = Duration.ofMillis(defaultCacheExpiration.toMillis)

  private val featuresCache          = new FeaturesCache(underlying.isFeatureActivated, invalidationPredicate = !_) // we don't keep knowledge about unactivated features
  private val assetDescriptionsCache = new AssetDescriptionsCache(underlying.assetDescription, cacheExpiration)

  override def realTimeBalanceChanges: Observable[WavesBlockchainClient.BalanceChanges]          = underlying.realTimeBalanceChanges
  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = underlying.spendableBalances(address, assets)
  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]]             = underlying.allAssetsSpendableBalance(address)

  override def isFeatureActivated(id: Short): Future[Boolean] = featuresCache.get(id) map Boolean2boolean

  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = assetDescriptionsCache.get(asset)

  override def hasScript(asset: Asset.IssuedAsset): Future[Boolean]                                     = underlying.hasScript(asset)
  override def runScript(asset: Asset.IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean]                       = underlying.hasScript(address)
  override def runScript(address: Address, input: Order): Future[RunScriptResult] = underlying.runScript(address, input)

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = underlying.wereForged(txIds)
  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean]          = underlying.broadcastTx(tx)

  override def forgedOrder(orderId: ByteStr): Future[Boolean] = underlying.forgedOrder(orderId)

  override def getNodeAddress: Future[InetAddress] = underlying.getNodeAddress

  override def close(): Future[Unit] = underlying.close()
}
