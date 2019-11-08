package com.wavesplatform.dex.db

import com.wavesplatform.database.DBExt
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.db.AssetsDB.Item
import com.wavesplatform.dex.effect.{FutureResult, liftValueAsync}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.iq80.leveldb.DB

// TODO cache
trait AssetsDB {
  def put(asset: IssuedAsset, item: Item): Unit
  def get(asset: IssuedAsset): Option[Item]
}

object AssetsDB {

  case class Item(name: String, decimals: Int)

  def apply(db: DB): AssetsDB = new LevelDBAssets(db)

  class LevelDBAssets(db: DB) extends AssetsDB {
    override def put(asset: IssuedAsset, record: Item): Unit = db.readWrite(_.put(MatcherKeys.asset(asset), Some(record)))
    override def get(asset: IssuedAsset): Option[Item]       = db.readOnly(_.get(MatcherKeys.asset(asset)))
  }

  val waves = AssetsDB.Item(
    name = AssetPair.WavesName,
    decimals = 8
  )

  val wavesLifted: FutureResult[Item] = liftValueAsync[Item] { waves }

  private val someWaves = Option(waves)

  final implicit class Ops(val self: AssetsDB) extends AnyVal {
    def get(asset: Asset): Option[Item]      = asset.fold(someWaves)(self.get)
    def unsafeGet(asset: Asset): Item        = get(asset).getOrElse(throw new RuntimeException(s"Unknown asset: ${AssetPair.assetIdStr(asset)}"))
    def unsafeGetDecimals(asset: Asset): Int = unsafeGet(asset).decimals
  }
}
