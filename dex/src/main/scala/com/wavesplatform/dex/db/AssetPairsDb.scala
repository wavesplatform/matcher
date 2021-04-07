package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.AssetPair

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.IteratorHasAsScala

trait AssetPairsDb[F[_]] {
  def add(pair: AssetPair): F[Unit]
  def remove(pair: AssetPair): F[Unit]
  def all(): F[Set[AssetPair]]
}

object AssetPairsDb {

  def levelDb[F[_]](levelDb: LevelDb[F]): AssetPairsDb[F] = new AssetPairsDb[F] {

    def add(pair: AssetPair): F[Unit] = levelDb.readWrite(_.put(DbKeys.assetPair(pair), ()))
    def remove(pair: AssetPair): F[Unit] = levelDb.readWrite(_.delete(DbKeys.assetPair(pair)))

    def all(): F[Set[AssetPair]] = levelDb.readOnly { ro =>
      val r = Set.newBuilder[AssetPair]

      ro.iterateOver(DbKeys.AssetPairsPrefix) { pair =>
        r += AssetPair.fromBytes(pair.getKey.drop(2))._1
      }

      r.result()
    }

  }

  def syncInMem: AssetPairsDb[Id] = new AssetPairsDb[Id] {
    private val storage = ConcurrentHashMap.newKeySet[AssetPair]()
    override def add(pair: AssetPair): Unit = storage.add(pair)
    override def remove(pair: AssetPair): Unit = storage.remove(pair)
    override def all(): Set[AssetPair] = storage.iterator().asScala.toSet
  }

  def asyncInMem(implicit ec: ExecutionContext): AssetPairsDb[Future] = new AssetPairsDb[Future] {
    private val storage = ConcurrentHashMap.newKeySet[AssetPair]()
    override def add(pair: AssetPair): Future[Unit] = Future(storage.add(pair))
    override def remove(pair: AssetPair): Future[Unit] = Future(storage.remove(pair))
    override def all(): Future[Set[AssetPair]] = Future(storage.iterator().asScala.toSet)
  }

}
