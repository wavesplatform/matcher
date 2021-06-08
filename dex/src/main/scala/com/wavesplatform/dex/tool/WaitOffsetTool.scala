package com.wavesplatform.dex.tool

import akka.actor.ActorSystem
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset
import com.wavesplatform.dex.settings.MatcherSettings

import java.util.concurrent.TimeoutException
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success}

object WaitOffsetTool extends ScorexLogging {

  def waitOffsetReached(
    getLastOffset: Deadline => Future[Offset],
    getLastProcessedOffset: => Offset,
    startingLastOffset: Offset,
    deadline: Deadline,
    settings: MatcherSettings
  )(implicit actorSystem: ActorSystem, ex: ExecutionContext): Future[Unit] = {
    val commandsPerSecond = settings.waitingQueueSettings.commandsPerSecond
    val interval = settings.waitingQueueSettings.checkInterval
    val maxWaitingTime = settings.waitingQueueSettings.maxWaitingTime

    def canProcessNewCommands(lastOffset: Offset, currentOffset: Offset): Boolean = {
      val leftTime = (lastOffset - currentOffset) / commandsPerSecond
      log.trace(s"Left time before matcher starts: $leftTime")
      leftTime <= maxWaitingTime.toSeconds
    }

    def processOffset(p: Promise[Unit], lastOffset: Offset): Unit = {
      val currentOffset = getLastProcessedOffset
      log.trace(s"offsets: $currentOffset >= $lastOffset, deadline: ${deadline.isOverdue()}")
      if (canProcessNewCommands(lastOffset, currentOffset))
        p.success(())
      else if (deadline.isOverdue())
        p.failure(new TimeoutException(s"Can't process all events in ${settings.startEventsProcessingTimeout.toMinutes} minutes"))
      else
        actorSystem.scheduler.scheduleOnce(interval)(loop(p))
    }

    def loop(p: Promise[Unit], lastOffsetOpt: Option[Offset] = None): Unit =
      lastOffsetOpt match {
        case Some(lastOffset) => processOffset(p, lastOffset)
        case None =>
          getLastOffset(deadline).onComplete {
            case Success(lastOffset) => processOffset(p, lastOffset)
            case Failure(ex) => p.failure(ex)
          }
      }

    val p = Promise[Unit]()
    loop(p, Some(startingLastOffset))
    p.future
  }

}
