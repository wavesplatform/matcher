package com.wavesplatform.dex.db

import cats.Id
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import com.wavesplatform.dex.db.leveldb.{Key, LevelDb}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.OrderBookSnapshot
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}

trait OrderBookSnapshotDb[F[_]] {
  def get(assetPair: AssetPair): F[Option[(Offset, OrderBookSnapshot)]]
  def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): F[Unit]
  def delete(assetPair: AssetPair): F[Unit]
}

object OrderBookSnapshotDb {

  def levelDb[F[_]](levelDb: LevelDb[F]): OrderBookSnapshotDb[F] = new OrderBookSnapshotDb[F] {

    override def get(assetPair: AssetPair): F[Option[(Offset, OrderBookSnapshot)]] = levelDb.readOnly { ro =>
      val (obOffsetKey, obKey) = keys(assetPair)
      (ro.get(obOffsetKey), ro.get(obKey)).tupled
    }

    override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): F[Unit] = levelDb.readWrite { rw =>
      val (obOffsetKey, obKey) = keys(assetPair)
      rw.put(obOffsetKey, Some(offset))
      newSnapshot.foreach(x => rw.put(obKey, Some(x)))
    }

    override def delete(assetPair: AssetPair): F[Unit] = levelDb.readWrite { rw =>
      val (obOffsetKey, obKey) = keys(assetPair)
      rw.delete(obOffsetKey)
      rw.delete(obKey)
    }

    private def keys(assetPair: AssetPair): (Key[Option[Offset]], Key[Option[OrderBookSnapshot]]) =
      (DbKeys.orderBookSnapshotOffset(assetPair), DbKeys.orderBookSnapshot(assetPair))

  }

  def syncInMem: OrderBookSnapshotDb[Id] = new OrderBookSnapshotDb[Id] {
    private val storage = new ConcurrentHashMap[AssetPair, (Offset, OrderBookSnapshot)]()

    override def get(assetPair: AssetPair): Option[(Offset, OrderBookSnapshot)] = Option(storage.get(assetPair))

    override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): Unit =
      storage.compute(assetPair, (_, current) => (offset, newSnapshot.getOrElse(current._2)))

    override def delete(assetPair: AssetPair): Unit = storage.remove(assetPair)
  }

  def asyncInMem(implicit ec: ExecutionContext): OrderBookSnapshotDb[Future] = new OrderBookSnapshotDb[Future] {
    private val storage = new ConcurrentHashMap[AssetPair, (Offset, OrderBookSnapshot)]()

    override def get(assetPair: AssetPair): Future[Option[(Offset, OrderBookSnapshot)]] =
      Future(Option(storage.get(assetPair)))

    override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): Future[Unit] =
      Future(storage.compute(assetPair, (_, current) => (offset, newSnapshot.getOrElse(current._2))))

    override def delete(assetPair: AssetPair): Future[Unit] =
      Future(storage.remove(assetPair))
  }

}
