package com.wavesplatform.dex.grpc.integration.clients.matcherext

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.caches.{AssetDescriptionsCache, FeaturesCache}
import com.wavesplatform.dex.grpc.integration.clients.domain.{BlockRef, BlockchainBalance, DiffIndex}
import com.wavesplatform.dex.grpc.integration.clients.{BroadcastResult, CheckedBroadcastResult, RunScriptResult}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

import java.time.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class MatcherExtensionCachingClient(underlying: MatcherExtensionClient, defaultCacheExpiration: FiniteDuration)(
  implicit grpcExecutionContext: ExecutionContext
) extends MatcherExtensionClient
    with ScorexLogging {

  private val cacheExpiration: Duration = Duration.ofMillis(defaultCacheExpiration.toMillis)

  private val featuresCache =
    new FeaturesCache(underlying.isFeatureActivated, invalidationPredicate = !_) // we don't keep knowledge about unactivated features
  private val assetDescriptionsCache = new AssetDescriptionsCache(underlying.assetDescription, cacheExpiration)

  override val utxEvents = underlying.utxEvents

  override def getOutgoingLeasing(address: Address): Future[Long] = underlying.getOutgoingLeasing(address)

  override def getAddressPartialRegularBalance(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    underlying.getAddressPartialRegularBalance(address, assets)

  override def getAddressFullRegularBalance(address: Address, excludeAssets: Set[Asset]): Future[Map[Asset, Long]] =
    underlying.getAddressFullRegularBalance(address, excludeAssets)

  override def getBalances(index: DiffIndex): Future[BlockchainBalance] = underlying.getBalances(index)

  override def isFeatureActivated(id: Short): Future[Boolean] = featuresCache.get(id) map Boolean2boolean

  override def assetDescription(asset: Asset.IssuedAsset): Future[Option[BriefAssetDescription]] = assetDescriptionsCache.get(asset)

  override def hasScript(asset: Asset.IssuedAsset): Future[Boolean] = underlying.hasScript(asset)
  override def runScript(asset: Asset.IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] = underlying.hasScript(address)
  override def runScript(address: Address, input: Order): Future[RunScriptResult] = underlying.runScript(address, input)

  override def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = underlying.areKnown(txIds)
  override def broadcastTx(tx: ExchangeTransaction): Future[BroadcastResult] = underlying.broadcastTx(tx)
  override def checkedBroadcastTx(tx: ExchangeTransaction): Future[CheckedBroadcastResult] = underlying.checkedBroadcastTx(tx)

  override def isOrderConfirmed(orderId: ByteStr): Future[Boolean] = underlying.isOrderConfirmed(orderId)

  override def currentBlockInfo: Future[BlockRef] = underlying.currentBlockInfo

  override def close(): Future[Unit] = underlying.close()
}
