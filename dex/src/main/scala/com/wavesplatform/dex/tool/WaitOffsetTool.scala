package com.wavesplatform.dex.tool

import akka.actor.Scheduler
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.WaitingOffsetToolSettings
import com.wavesplatform.dex.tool.WaitOffsetTool.OffsetAndTime

import java.util.concurrent.TimeoutException
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success}
import scala.concurrent.duration._

private[tool] trait WaitOffsetTool extends ScorexLogging {

  def waitOffsetReached(
    getLastOffset: => Future[Offset],
    getLastProcessedOffset: => Offset,
    initialLastOffset: Offset,
    deadline: Deadline,
    settings: WaitingOffsetToolSettings,
    scheduler: Scheduler
  )(implicit ex: ExecutionContext): Future[Unit] = {
    val checkInterval = settings.checkInterval
    val maxWaitingTime = settings.maxWaitingTime

    def canProcessNewCommands(lastOffset: Offset, commandsPerSecond: Double)(currentOffset: Offset): Boolean =
      (lastOffset - currentOffset) / commandsPerSecond <= maxWaitingTime.toSeconds

    def processOffset(p: Promise[Unit], lastOffset: Offset, canProcessNewCommandCondition: Offset => Boolean): Unit = {
      val currentOffset = getLastProcessedOffset
      log.trace(s"offsets: $currentOffset >= $lastOffset, deadline: ${deadline.isOverdue()}")
      val currentTime = System.nanoTime()
      if (canProcessNewCommandCondition(currentOffset))
        p.success(())
      else if (deadline.isOverdue())
        p.failure(new TimeoutException(s"Can't process all events in ${settings.queueProcessingTimeout.toMinutes} minutes"))
      else
        scheduler.scheduleOnce(checkInterval) {
          loop(p, OffsetAndTime(currentOffset, currentTime))
        }
    }

    def loop(p: Promise[Unit], prevOffsetAndTime: OffsetAndTime): Unit =
      getLastOffset.onComplete {
        case Success(lastOffset) =>
          val commandsPerSecond = calcCommandsPerSecond(prevOffsetAndTime, getLastProcessedOffset)
          processOffset(p, lastOffset, canProcessNewCommands(lastOffset, commandsPerSecond))
        case Failure(ex) => p.failure(ex)
      }

    val p = Promise[Unit]()
    processOffset(p, initialLastOffset, currentOffset => currentOffset >= initialLastOffset)
    p.future
  }

  protected def calcCommandsPerSecond(prevOffsetAndTime: OffsetAndTime, lastProcessedOffset: Offset): Double

}

object WaitOffsetTool extends WaitOffsetTool {

  final case class OffsetAndTime(offset: Offset, time: Long)

  override def calcCommandsPerSecond(prevOffsetAndTime: OffsetAndTime, lastProcessedOffset: Offset): Double = {
    val timeDifference = (System.nanoTime() - prevOffsetAndTime.time).nano.toSeconds
    val commandDifference = lastProcessedOffset - prevOffsetAndTime.offset
    commandDifference.toDouble / timeDifference.toDouble
  }

}
