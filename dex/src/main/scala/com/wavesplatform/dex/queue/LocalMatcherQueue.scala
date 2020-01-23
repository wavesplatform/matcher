package com.wavesplatform.dex.queue

import java.util.concurrent.Executors
import java.util.{Timer, TimerTask}

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.dex.LocalQueueStore
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.LocalMatcherQueue._
import com.wavesplatform.dex.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.dex.time.Time

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.util.control.NonFatal

class LocalMatcherQueue(settings: Settings, store: LocalQueueStore, time: Time) extends MatcherQueue with ScorexLogging {

  @volatile private var lastUnreadOffset: QueueEventWithMeta.Offset = -1L

  private val executor = Executors.newSingleThreadExecutor {
    new ThreadFactoryBuilder()
      .setDaemon(true)
      .setNameFormat("queue-local-consumer-%d")
      .build()
  }

  private implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

  private val timer = new Timer("local-dex-queue", true)
  private val producer: Producer = {
    val r = if (settings.enableStoring) new LocalProducer(store, time) else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: Seq[QueueEventWithMeta] => Future[Unit]): Unit = {
    if (settings.cleanBeforeConsume) store.dropUntil(fromOffset)

    def runOnce(from: QueueEventWithMeta.Offset): Future[QueueEventWithMeta.Offset] = {
      val requests = store.getFrom(from, settings.maxElementsPerPoll)
      if (requests.isEmpty) Future.successful(from)
      else {
        val newOffset = requests.last.offset + 1
        log.trace(s"Read ${newOffset - from} events")
        process(requests).map(_ => newOffset)
      }
    }

    val pollingInterval = settings.pollingInterval.toNanos
    def loop(from: QueueEventWithMeta.Offset): Unit = {
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
          val diff  = System.nanoTime() - start
          val delay = math.max(pollingInterval - diff, 0L) / 1000000 // to millis
          timer.schedule(new TimerTask {
            override def run(): Unit = loop(lastUnreadOffset)
          }, delay)
        }
    }

    loop(fromOffset)
  }

  override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = producer.storeEvent(event)

  override def lastEventOffset: Future[QueueEventWithMeta.Offset] = Future(store.newestOffset.getOrElse(-1L))

  override def close(timeout: FiniteDuration): Unit = {
    timer.cancel()
    producer.close(timeout)
  }
}

object LocalMatcherQueue {
  case class Settings(enableStoring: Boolean, pollingInterval: FiniteDuration, maxElementsPerPoll: Int, cleanBeforeConsume: Boolean)

  private class LocalProducer(store: LocalQueueStore, time: Time) extends Producer {
    private val executor = Executors.newSingleThreadExecutor {
      new ThreadFactoryBuilder()
        .setDaemon(true)
        .setNameFormat("queue-local-producer-%d")
        .build()
    }

    private implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

    override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = {
      val p = Promise[QueueEventWithMeta]
      // Need to guarantee the order
      executor.submit(new Runnable {
        override def run(): Unit = {
          val ts     = time.correctedTime()
          val offset = store.enqueue(event, time.correctedTime())
          p.success(QueueEventWithMeta(offset, ts, event))
        }
      })
      p.future.map(Some(_))
    }

    override def close(timeout: FiniteDuration): Unit = executor.shutdown()
  }
}
