package com.wavesplatform.dex.tool

import cats.Id
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.db.DbKeys
import com.wavesplatform.dex.db.leveldb.{openDb, LevelDb}
import cats.syntax.either._
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import org.iq80.leveldb.DB

object AssetCacheCleaner {

  def cleanAssets(dataDirectory: String): ErrorOr[Unit] = Either.catchNonFatal {
    val db: DB = openDb(dataDirectory)
    LevelDb.sync(db)
  }.flatMap(cleanAssets)
    .leftMap(ex => s"Error during removing assets: ${ex.getWithStackTrace}")

  def cleanAssets(levelDb: LevelDb[Id]): Either[Throwable, Unit] = Either.catchNonFatal {
    levelDb.readWrite { rw =>
      rw.iterateOver(DbKeys.AssetPrefix) { entity =>
        rw.delete(entity.getKey)
      }
    }
  }

}
