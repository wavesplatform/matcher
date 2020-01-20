package com.wavesplatform.dex.db.leveldb

import com.wavesplatform.dex.metrics.LevelDBStats
import com.wavesplatform.dex.metrics.LevelDBStats.DbHistogramExt
import org.iq80.leveldb.{DB, ReadOptions, WriteBatch}

class RW(db: DB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) {

  def put[V](key: Key[V], value: V): Unit = {
    val bytes = key.encode(value)
    LevelDBStats.write.recordTagged(key, bytes)
    batch.put(key.keyBytes, bytes)
  }

  /** Because of how leveldb batches work, you can increment a specific value only once! */
  def inc(key: Key[Int]): Int = {
    val newValue = get(key) + 1
    put(key, newValue)
    newValue
  }

  def delete[V](key: Key[V]): Unit = batch.delete(key.keyBytes)
}
