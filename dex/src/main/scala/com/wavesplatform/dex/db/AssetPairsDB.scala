package com.wavesplatform.dex.db

import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.db.leveldb.DBExt
import com.wavesplatform.dex.domain.asset.AssetPair
import org.iq80.leveldb.DB

trait AssetPairsDB {
  def add(pair: AssetPair): Unit
  def remove(pair: AssetPair): Unit
  def all(): Set[AssetPair]
}

object AssetPairsDB {

  def apply(db: DB): AssetPairsDB = new AssetPairsDB {

    def add(pair: AssetPair): Unit    = db.readWrite(_.put(MatcherKeys.assetPair(pair), ()))
    def remove(pair: AssetPair): Unit = db.readWrite(_.delete(MatcherKeys.assetPair(pair)))

    def all(): Set[AssetPair] = db.readOnly { ro =>
      val r = Set.newBuilder[AssetPair]

      ro.iterateOver(MatcherKeys.AssetPairsPrefix) { pair =>
        r += AssetPair.fromBytes(pair.getKey.drop(2))
      }

      r.result()
    }
  }
}
