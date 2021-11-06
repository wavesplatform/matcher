package com.wavesplatform.dex.db

import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import com.google.common.primitives.Longs
import com.wavesplatform.dex.db.leveldb.{Key, LevelDb}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.OrderBookSnapshot
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset

import java.nio.ByteBuffer

trait OrderBookSnapshotDb[F[_]] {
  def get(assetPair: AssetPair): F[Option[(Offset, OrderBookSnapshot)]]
  def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]): F[Unit]
  def delete(assetPair: AssetPair): F[Unit]
  def iterateOffsets(pred: AssetPair => Boolean): F[Map[AssetPair, Offset]]
  def iterateSnapshots(pred: AssetPair => Boolean): F[Map[AssetPair, OrderBookSnapshot]]
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

    def iterateOffsets(pred: AssetPair => Boolean): F[Map[AssetPair, Offset]] = levelDb.readOnly { ro =>
      val m = Map.newBuilder[AssetPair, Offset]
      ro.iterateOver(DbKeys.OrderBookSnapshotOffsetPrefix) { entry =>
        val pair = AssetPair.fromBytes(entry.getKey.drop(2))._1
        if (pred(pair))
          m.addOne(pair -> Longs.fromByteArray(entry.getValue))
      }
      m.result()
    }

    def iterateSnapshots(pred: AssetPair => Boolean): F[Map[AssetPair, OrderBookSnapshot]] = levelDb.readOnly { ro =>
      val m = Map.newBuilder[AssetPair, OrderBookSnapshot]
      ro.iterateOver(DbKeys.OrderBookSnapshotPrefix) { entry =>
        val pair = AssetPair.fromBytes(entry.getKey.drop(2))._1
        if (pred(pair))
          m.addOne(pair -> OrderBookSnapshot.fromBytes(ByteBuffer.wrap(entry.getValue)))
      }
      m.result()
    }

    private def keys(assetPair: AssetPair): (Key[Option[Offset]], Key[Option[OrderBookSnapshot]]) =
      (DbKeys.orderBookSnapshotOffset(assetPair), DbKeys.orderBookSnapshot(assetPair))

  }

}
