package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.db.leveldb.{LevelDBFactory, LevelDb}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.util.Implicits._
import com.wavesplatform.dex.util.TestHelpers
import monix.reactive.subjects.Subject
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterEach, Suite}

import java.nio.file.Files
import scala.concurrent.Future

trait WithDB extends BeforeAndAfterEach { this: Suite =>

  implicit protected val executionContext = scala.concurrent.ExecutionContext.Implicits.global

  private val path = Files.createTempDirectory("lvl").toAbsolutePath
  private var currentDBInstance: DB = _

  def db: DB = currentDBInstance
  def asyncLevelDb: LevelDb[Future] = LevelDb.async(db)

  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = Subject.empty

  override def beforeEach(): Unit = {
    currentDBInstance = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    super.beforeEach()
  }

  override def afterEach(): Unit =
    try {
      super.afterEach()
      db.close()
    } finally TestHelpers.deleteRecursively(path)

  protected def tempDb(f: DB => Any): Any = {
    val path = Files.createTempDirectory("lvl-temp").toAbsolutePath
    val db = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    try f(db)
    finally {
      try db.close()
      catch {
        case _: Throwable =>
      }
      TestHelpers.deleteRecursively(path)
    }
  }

  protected def tempLevelDb(f: LevelDb[Id] => Any): Any = tempDb(db => f(LevelDb.sync(db)))

}
