package com.wavesplatform.dex.db

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.DBExt
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.db.AssetsDB.Item
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

trait AssetsDB {
  def put(asset: IssuedAsset, item: Item): Unit
  def get(asset: IssuedAsset): Option[Item]
}

object AssetsDB {
  case class Item(name: ByteStr, decimals: Int)

  def apply(db: DB): AssetsDB = new AssetsDB with ScorexLogging {
    override def put(asset: IssuedAsset, record: Item): Unit = {
      log.info(s"put(${AssetPair.assetIdStr(asset)}, $record)")
      db.readWrite(_.put(MatcherKeys.asset(asset), Some(record)))
    }
    override def get(asset: IssuedAsset): Option[Item] = {
      val r = db.readOnly(_.get(MatcherKeys.asset(asset)))
      log.info(s"get(${AssetPair.assetIdStr(asset)}): $r")
      r
    }
  }
}
