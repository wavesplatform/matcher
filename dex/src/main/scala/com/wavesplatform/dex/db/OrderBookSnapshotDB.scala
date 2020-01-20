package com.wavesplatform.dex.db

import java.util.concurrent.ConcurrentHashMap

import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply.catsSyntaxTuple2Semigroupal
import com.wavesplatform.dex.MatcherKeys
import com.wavesplatform.dex.db.leveldb.{DBExt, Key}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.OrderBook.Snapshot
import com.wavesplatform.dex.queue.QueueEventWithMeta.Offset
import org.iq80.leveldb.DB

trait OrderBookSnapshotDB {
  def get(assetPair: AssetPair): Option[(Offset, Snapshot)]
  def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[Snapshot]): Unit
  def delete(assetPair: AssetPair): Unit
}

object OrderBookSnapshotDB {
  def apply(db: DB): OrderBookSnapshotDB = new OrderBookSnapshotDB {
    override def get(assetPair: AssetPair): Option[(Offset, Snapshot)] = db.readOnly { ro =>
      val (obOffsetKey, obKey) = keys(assetPair)
      (ro.get(obOffsetKey), ro.get(obKey)).tupled
    }

    override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[Snapshot]): Unit = db.readWrite { rw =>
      val (obOffsetKey, obKey) = keys(assetPair)
      rw.put(obOffsetKey, Some(offset))
      newSnapshot.foreach(x => rw.put(obKey, Some(x)))
    }

    override def delete(assetPair: AssetPair): Unit = db.readWrite { rw =>
      val (obOffsetKey, obKey) = keys(assetPair)
      rw.delete(obOffsetKey)
      rw.delete(obKey)
    }

    private def keys(assetPair: AssetPair): (Key[Option[Offset]], Key[Option[Snapshot]]) =
      (MatcherKeys.orderBookSnapshotOffset(assetPair), MatcherKeys.orderBookSnapshot(assetPair))
  }

  def inMem: OrderBookSnapshotDB = new OrderBookSnapshotDB {
    private val snapshots = new ConcurrentHashMap[AssetPair, (Offset, Snapshot)]()

    override def get(assetPair: AssetPair): Option[(Offset, Snapshot)] = Option(snapshots.get(assetPair))

    override def update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[Snapshot]): Unit =
      snapshots.compute(assetPair, (_, current) => (offset, newSnapshot.getOrElse(current._2)))

    override def delete(assetPair: AssetPair): Unit = snapshots.remove(assetPair)
  }
}
