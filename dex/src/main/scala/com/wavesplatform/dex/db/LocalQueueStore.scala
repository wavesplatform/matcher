package com.wavesplatform.dex.db

import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.dex.db.DbKeys._
import com.wavesplatform.dex.db.leveldb.{LevelDb, ReadOnlyDb}
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

trait LocalQueueStore[F[_]] {

  def enqueue(command: ValidatedCommand, timestamp: Long): F[ValidatedCommandWithMeta.Offset]

  def getFrom(offset: ValidatedCommandWithMeta.Offset, maxElements: Int): F[List[ValidatedCommandWithMeta]]

  def oldestOffset: F[Option[ValidatedCommandWithMeta.Offset]]

  def newestOffset: F[Option[ValidatedCommandWithMeta.Offset]]

  def dropUntil(offset: ValidatedCommandWithMeta.Offset): F[Unit]

}

object LocalQueueStore {

  def levelDb[F[_]](levelDb: LevelDb[F]): LocalQueueStore[F] = new LevelDbLocalQueueStore(levelDb)

}

/**
 * Note, this works only for single thread pool!
 */
class LevelDbLocalQueueStore[F[_]](levelDb: LevelDb[F]) extends LocalQueueStore[F] {

  private val inMemQueue = new ConcurrentLinkedQueue[ValidatedCommandWithMeta]
  private val startInMemOffset = new AtomicReference[Option[ValidatedCommandWithMeta.Offset]](None)

  override def enqueue(command: ValidatedCommand, timestamp: Long): F[ValidatedCommandWithMeta.Offset] =
    levelDb.readWrite { rw =>
      val offset = getAndIncrementNewestIdx(rw)
      val eventKey = lpqElement(offset)
      val x = ValidatedCommandWithMeta(offset, timestamp, command)

      rw.put(eventKey, Some(x))
      rw.put(lqNewestIdx, offset)

      // We need to process it here, otherwise we have a race condition, thus a wrong order of messages in the queue
      inMemQueue.add(x)
      startInMemOffset.compareAndSet(None, Some(offset))

      offset
    }

  override def getFrom(offset: ValidatedCommandWithMeta.Offset, maxElements: Int): F[List[ValidatedCommandWithMeta]] =
    levelDb.readOnly { ro =>
      // We need to process it here, otherwise we have a race condition, thus a wrong order of messages in the queue
      if (startInMemOffset.get().exists(_ <= offset))
        if (inMemQueue.isEmpty) Nil
        else {
          var xs = List.empty[ValidatedCommandWithMeta]
          var added = 0

          while (!inMemQueue.isEmpty && added < maxElements) Option(inMemQueue.poll()).foreach { x =>
            xs ::= x
            added += 1
          }

          xs.reverse
        }
      else
        ro.readList(LqElementKeyName, LqElementPrefixBytes, lpqElement(math.max(offset, 0)).keyBytes, maxElements) { e =>
          val offset = Longs.fromByteArray(e.getKey.slice(Shorts.BYTES, Shorts.BYTES + Longs.BYTES))
          lpqElement(offset).parse(e.getValue).getOrElse(throw new RuntimeException(s"Can't find a queue event at $offset"))
        }
    }

  override def oldestOffset: F[Option[ValidatedCommandWithMeta.Offset]] =
    levelDb.readOnly { ro =>
      ro
        .readList(LqElementKeyName, LqElementPrefixBytes, lpqElement(0).keyBytes, 1) { e =>
          Longs.fromByteArray(e.getKey.slice(Shorts.BYTES, Shorts.BYTES + Longs.BYTES))
        }
        .headOption
    }

  override def newestOffset: F[Option[ValidatedCommandWithMeta.Offset]] = levelDb.readOnly { ro =>
    val eventKey = lpqElement(getNewestIdx(ro))
    ro.get(eventKey).map(_.offset)
  }

  override def dropUntil(offset: ValidatedCommandWithMeta.Offset): F[Unit] =
    levelDb.readWrite { rw =>
      val idx = rw.get(lqOldestIdx)
      val oldestIdx = math.max(idx, 0)
      (oldestIdx until offset).foreach { offset =>
        rw.delete(lpqElement(offset))
      }
      rw.put(lqOldestIdx, offset)
    }

  private def getNewestIdx(ro: ReadOnlyDb): Long = ro.get(lqNewestIdx)

  private def getAndIncrementNewestIdx(ro: ReadOnlyDb): Long = getNewestIdx(ro) + 1

}
