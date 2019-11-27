package com.wavesplatform.dex.db

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.database.DBExt
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.effect.{FutureResult, liftValueAsync}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.iq80.leveldb.DB

trait AssetsStorage {
  def put(asset: IssuedAsset, item: BriefAssetDescription): Unit
  def get(asset: IssuedAsset): Option[BriefAssetDescription]
}

object AssetsStorage {

  val wavesLifted: FutureResult[BriefAssetDescription] = liftValueAsync { BriefAssetDescription.wavesDescription }

  def cache(inner: AssetsStorage): AssetsStorage = new AssetsStorage {

    private val assetsCache = new ConcurrentHashMap[Asset, BriefAssetDescription]

    def put(asset: Asset.IssuedAsset, item: BriefAssetDescription): Unit = {
      inner.put(asset, item)
      assetsCache.putIfAbsent(asset, item)
    }

    def get(asset: Asset.IssuedAsset): Option[BriefAssetDescription] = Option {
      assetsCache.computeIfAbsent(asset, inner.get(_).orNull)
    }
  }

  def levelDB(db: DB): AssetsStorage = new AssetsStorage {
    def put(asset: IssuedAsset, record: BriefAssetDescription): Unit = db.readWrite(_.put(MatcherKeys.asset(asset), Some(record)))
    def get(asset: IssuedAsset): Option[BriefAssetDescription]       = db.readOnly(_.get(MatcherKeys.asset(asset)))
  }

  final implicit class Ops(val self: AssetsStorage) extends AnyVal {

    def get(asset: Asset): Option[BriefAssetDescription] = asset.fold(BriefAssetDescription.someWavesDescription)(self.get)

    def unsafeGet(asset: Asset): BriefAssetDescription = {
      get(asset).getOrElse(throw new RuntimeException(s"Unknown asset: ${AssetPair.assetIdStr(asset)}"))
    }

    def unsafeGetDecimals(asset: Asset): Int      = unsafeGet(asset).decimals
    def unsafeGetHasScript(asset: Asset): Boolean = unsafeGet(asset).hasScript
  }
}
