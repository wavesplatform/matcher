package com.wavesplatform.dex.queue

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.dex.db.LocalQueueStore
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.LocalMatcherQueue._
import com.wavesplatform.dex.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.time.Time

import java.util.concurrent.Executors
import java.util.{Timer, TimerTask}
import scala.concurrent._
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class LocalMatcherQueue(settings: Settings, store: LocalQueueStore[Future], time: Time) extends MatcherQueue with ScorexLogging {

  @volatile private var lastUnreadOffset: ValidatedCommandWithMeta.Offset = -1L

  private val executor = Executors.newSingleThreadExecutor {
    new ThreadFactoryBuilder()
      .setDaemon(false)
      .setNameFormat("queue-local-consumer-%d")
      .build()
  }

  implicit private val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

  private val timer = new Timer("local-dex-queue", true)

  private val producer: Producer = {
    val r = if (settings.enableStoring) new LocalProducer(store, time) else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  override def startConsume(fromOffset: ValidatedCommandWithMeta.Offset, process: List[ValidatedCommandWithMeta] => Future[Unit]): Unit = {
    if (settings.cleanBeforeConsume) store.dropUntil(fromOffset).onComplete {
      case Success(_) =>
      case Failure(e) => log.error(s"Can't drop messages from $fromOffset", e)
    }

    def runOnce(from: ValidatedCommandWithMeta.Offset): Future[ValidatedCommandWithMeta.Offset] =
      store.getFrom(from, settings.maxElementsPerPoll).flatMap { requests =>
        if (requests.isEmpty) Future.successful(from)
        else {
          val newOffset = requests.last.offset + 1
          log.trace(s"Read ${newOffset - from} commands")
          process(requests).map(_ => newOffset)
        }
      }

    val pollingInterval = settings.pollingInterval.toNanos
    def loop(from: ValidatedCommandWithMeta.Offset): Unit = {
      val start = System.nanoTime()
      runOnce(from)
        .recover {
          case NonFatal(e) =>
            // Actually this should not happen. The Future[_] type is not powerful to express error-less computations
            log.error("Can't process messages, trying again", e)
            from
        }
        .map { nextStartOffset =>
          lastUnreadOffset = nextStartOffset
          val diff = System.nanoTime() - start
          val delay = math.max(pollingInterval - diff, 0L) / 1000000 // to millis
          timer.schedule(
            new TimerTask {
              override def run(): Unit = loop(lastUnreadOffset)
            },
            delay
          )
        }
    }

    loop(fromOffset)
  }

  override def store(command: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]] = producer.store(command)

  override def firstOffset: Future[Offset] = store.oldestOffset.map(_.getOrElse(-1L))

  override def lastOffset: Future[ValidatedCommandWithMeta.Offset] = store.newestOffset.map(_.getOrElse(-1L))

  override def close(timeout: FiniteDuration): Future[Unit] =
    Future {
      blocking {
        timer.cancel()
        producer.close(timeout)
        executor.shutdown()
      }
    }(scala.concurrent.ExecutionContext.global)

}

object LocalMatcherQueue {
  case class Settings(enableStoring: Boolean, pollingInterval: FiniteDuration, maxElementsPerPoll: Int, cleanBeforeConsume: Boolean)

  private class LocalProducer(store: LocalQueueStore[Future], time: Time) extends Producer {

    // Need to guarantee the order
    private val executor = Executors.newSingleThreadExecutor {
      new ThreadFactoryBuilder()
        .setDaemon(true)
        .setNameFormat("queue-local-producer-%d")
        .build()
    }

    implicit private val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

    override def store(command: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]] = {
      val ts = time.correctedTime()
      store
        .enqueue(command, ts)
        .map(offset => Option(ValidatedCommandWithMeta(offset, ts, command)))
    }

    override def close(timeout: FiniteDuration): Unit = executor.shutdown()
  }

}
