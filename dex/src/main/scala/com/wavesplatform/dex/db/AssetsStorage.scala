package com.wavesplatform.dex.db

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.dex.db.leveldb.DBExt
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.effect.{FutureResult, liftValueAsync}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import org.iq80.leveldb.DB

trait AssetsStorage {
  def put(asset: IssuedAsset, item: BriefAssetDescription): Unit
  def get(asset: IssuedAsset): Option[BriefAssetDescription]
  def contains(asset: IssuedAsset): Boolean = get(asset).nonEmpty
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

    override def contains(asset: IssuedAsset): Boolean = assetsCache.contains(asset)
  }

  def levelDB(db: DB): AssetsStorage = new AssetsStorage {
    def put(asset: IssuedAsset, record: BriefAssetDescription): Unit = db.readWrite(_.put(DbKeys.asset(asset), Some(record)))
    def get(asset: IssuedAsset): Option[BriefAssetDescription]       = db.readOnly(_.get(DbKeys.asset(asset)))
  }

  def inMem: AssetsStorage = new AssetsStorage {
    private val assetsCache                                        = new ConcurrentHashMap[Asset, BriefAssetDescription]
    def put(asset: IssuedAsset, item: BriefAssetDescription): Unit = assetsCache.putIfAbsent(asset, item)
    def get(asset: IssuedAsset): Option[BriefAssetDescription]     = Option(assetsCache get asset)
  }

  final implicit class Ops(val self: AssetsStorage) extends AnyVal {

    def get(asset: Asset): Option[BriefAssetDescription] = asset.fold(BriefAssetDescription.someWavesDescription)(self.get)

    def unsafeGet(asset: Asset): BriefAssetDescription = {
      get(asset).getOrElse(throw new RuntimeException(s"Unknown asset: ${asset.toString}"))
    }

    def unsafeGetDecimals(asset: Asset): Int      = unsafeGet(asset).decimals
    def unsafeGetHasScript(asset: Asset): Boolean = unsafeGet(asset).hasScript
  }
}
