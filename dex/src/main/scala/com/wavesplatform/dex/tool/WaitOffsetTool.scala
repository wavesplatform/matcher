package com.wavesplatform.dex.tool

import akka.actor.Scheduler
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.MatcherSettings

import java.util.concurrent.TimeoutException
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success}
import scala.concurrent.duration._

object WaitOffsetTool extends ScorexLogging {

  case class TimeCheckTag(currentOffset: Offset, time: Long)

  private def calculateCommandsPerSecond(lastCheckTime: Option[TimeCheckTag], lastProcessedOffset: Offset): Option[Double] = {
    lastCheckTime.map { lastTimeCheck =>
      val timeDifference = (System.nanoTime() - lastTimeCheck.time).nano.toSeconds
      val commandDifference = lastProcessedOffset - lastTimeCheck.currentOffset
      commandDifference/timeDifference
    }
  }

  def waitOffsetReached(
    getLastOffset: Deadline => Future[Offset],
    getLastProcessedOffset: => Offset,
    startingLastOffset: Offset,
    deadline: Deadline,
    settings: MatcherSettings
  )(implicit scheduler: Scheduler, ex: ExecutionContext): Future[Unit] = {
    val interval = settings.waitingQueue.checkInterval
    val maxWaitingTime = settings.waitingQueue.maxWaitingTime

    def canProcessNewCommands(lastOffset: Offset, currentOffset: Offset, commandsPerSecond: Option[Double]): Boolean =
      commandsPerSecond.fold (currentOffset >= lastOffset) { k =>
        (lastOffset - currentOffset) / k <= maxWaitingTime.toSeconds
      }

    def processOffset(p: Promise[Unit], lastOffset: Offset, commandsPerSecond: Option[Double]): Unit = {
      val currentOffset = getLastProcessedOffset
      log.trace(s"offsets: $currentOffset >= $lastOffset, deadline: ${deadline.isOverdue()}")
      val currentTime = System.nanoTime()
      if (canProcessNewCommands(lastOffset, currentOffset, commandsPerSecond))
        p.success(())
      else if (deadline.isOverdue())
        p.failure(new TimeoutException(s"Can't process all events in ${settings.startEventsProcessingTimeout.toMinutes} minutes"))
      else
        scheduler.scheduleOnce(interval) {
          loop(p, Some(TimeCheckTag(currentOffset, currentTime)))
        }
    }

    def loop(p: Promise[Unit], lastCheckTime: Option[TimeCheckTag], lastOffsetOpt: Option[Offset] = None): Unit =
      lastOffsetOpt match {
        case Some(lastOffset) => processOffset(p, lastOffset, calculateCommandsPerSecond(lastCheckTime, getLastProcessedOffset))
        case None =>
          getLastOffset(deadline).onComplete {
            case Success(lastOffset) => processOffset(p, lastOffset, calculateCommandsPerSecond(lastCheckTime, getLastProcessedOffset))
            case Failure(ex) => p.failure(ex)
          }
      }

    val p = Promise[Unit]()
    loop(p, None, Some(startingLastOffset))
    p.future
  }

}
