package com.wavesplatform.dex.db.leveldb

import com.wavesplatform.dex.metrics.LevelDBStats
import com.wavesplatform.dex.metrics.LevelDBStats.DbHistogramExt
import org.iq80.leveldb.{DB, DBIterator, ReadOptions}

import scala.annotation.tailrec

class ReadOnlyDb(db: DB, readOptions: ReadOptions) {

  def get[V](key: Key[V]): V = {
    val bytes = db.get(key.keyBytes, readOptions)
    LevelDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(key.keyBytes, readOptions)
    LevelDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  def iterator: DBIterator = db.iterator(readOptions)
  def iterateOver(prefix: Short)(f: DBEntry => Unit): Unit = db.iterateOver(prefix)(f)

  def readList[T](keyName: String, prefix: Array[Byte], seek: Array[Byte], n: Int)(deserialize: DBEntry => T): List[T] =
    read[List, T](keyName, prefix, seek, n)(deserialize)(Nil, (xs, x) => x :: xs).reverse

  def read[C[_], T](keyName: String, prefix: Array[Byte], seek: Array[Byte], n: Int)(deserialize: DBEntry => T)(
    empty: C[T],
    append: (C[T], T) => C[T]
  ): C[T] = {
    val iter = iterator
    @tailrec def loop(aux: C[T], restN: Int, totalBytesRead: Long): (C[T], Long) =
      if (restN > 0 && iter.hasNext) {
        val elem = iter.next()
        if (elem.getKey.startsWith(prefix)) loop(append(aux, deserialize(elem)), restN - 1, totalBytesRead + elem.getValue.length)
        else (aux, totalBytesRead)
      } else (aux, totalBytesRead)

    try {
      iter.seek(seek)
      val (r, totalBytesRead) = loop(empty, n, 0)
      LevelDBStats.read.recordTagged(keyName, totalBytesRead)
      r
    } finally iter.close()
  }

}
