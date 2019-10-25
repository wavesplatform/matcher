package com.wavesplatform.dex.db

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.DBExt
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.iq80.leveldb.DB

trait AssetsDB {
  def add(asset: IssuedAsset, name: ByteStr, decimals: Int): Unit
}

object AssetsDB {
  def apply(db: DB): AssetsDB = new AssetsDB {
    override def add(asset: IssuedAsset, name: ByteStr, decimals: Int): Unit = db.readWrite(_.put(MatcherKeys.asset(asset), (name, decimals)))
  }
}
