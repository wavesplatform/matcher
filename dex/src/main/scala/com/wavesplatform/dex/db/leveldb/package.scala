package com.wavesplatform.dex.db

import java.io.File
import java.util.{Map => JMap}

import com.google.common.primitives.Shorts
import com.wavesplatform.dex.domain.utils.ScorexLogging
import org.iq80.leveldb.{DB, Options, ReadOptions}

package object leveldb extends ScorexLogging {

  def openDB(path: String, recreate: Boolean = false): DB = {

    log.debug(s"Open DB at $path")

    val file = new File(path)
    val options = new Options().createIfMissing(true).paranoidChecks(true)

    if (recreate)
      LevelDBFactory.factory.destroy(file, options)

    file.getAbsoluteFile.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

  final type DBEntry = JMap.Entry[Array[Byte], Array[Byte]]

  implicit class DBExt(val db: DB) extends AnyVal {

    def readOnly[A](f: ReadOnlyDB => A): A = {
      val snapshot = db.getSnapshot
      try f(new ReadOnlyDB(db, new ReadOptions().snapshot(snapshot)))
      finally snapshot.close()
    }

    /**
     * @note Runs operations in batch, so keep in mind, that previous changes don't appear lately in f
     */
    def readWrite[A](f: RW => A): A = {
      val snapshot = db.getSnapshot
      val readOptions = new ReadOptions().snapshot(snapshot)
      val batch = db.createWriteBatch()
      val rw = new RW(db, readOptions, batch)
      try {
        val r = f(rw)
        db.write(batch)
        r
      } finally {
        batch.close()
        snapshot.close()
      }
    }

    def get[A](key: Key[A]): A = key.parse(db.get(key.keyBytes))
    def has(key: Key[_]): Boolean = db.get(key.keyBytes) != null

    def iterateOver(prefix: Short)(f: DBEntry => Unit): Unit = iterateOver(Shorts.toByteArray(prefix))(f)

    def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit = {
      val iterator = db.iterator()
      try {
        iterator.seek(prefix)
        while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)) f(iterator.next())
      } finally iterator.close()
    }

  }

}
