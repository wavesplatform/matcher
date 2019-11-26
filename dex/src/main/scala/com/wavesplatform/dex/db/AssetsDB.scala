package com.wavesplatform.dex.db

import com.wavesplatform.database.DBExt
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.effect.{FutureResult, liftValueAsync}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.iq80.leveldb.DB

// TODO cache
trait AssetsDB {
  def put(asset: IssuedAsset, item: BriefAssetDescription): Unit
  def get(asset: IssuedAsset): Option[BriefAssetDescription]
}

object AssetsDB {

  def apply(db: DB): AssetsDB = new LevelDBAssets(db)

  class LevelDBAssets(db: DB) extends AssetsDB {
    override def put(asset: IssuedAsset, record: BriefAssetDescription): Unit = db.readWrite(_.put(MatcherKeys.asset(asset), Some(record)))
    override def get(asset: IssuedAsset): Option[BriefAssetDescription]       = db.readOnly(_.get(MatcherKeys.asset(asset)))
  }

  val waves                                            = BriefAssetDescription(name = AssetPair.WavesName, decimals = 8, hasScript = false)
  val wavesLifted: FutureResult[BriefAssetDescription] = liftValueAsync[BriefAssetDescription] { waves }

  private val someWaves = Option(waves)

  final implicit class Ops(val self: AssetsDB) extends AnyVal {

    def get(asset: Asset): Option[BriefAssetDescription] = asset.fold(someWaves)(self.get)

    def unsafeGet(asset: Asset): BriefAssetDescription = {
      get(asset).getOrElse(throw new RuntimeException(s"Unknown asset: ${AssetPair.assetIdStr(asset)}"))
    }

    def unsafeGetDecimals(asset: Asset): Int      = unsafeGet(asset).decimals
    def unsafeGetHasScript(asset: Asset): Boolean = unsafeGet(asset).hasScript
  }
}
