package com.wavesplatform.dex.queue

import java.util.concurrent.TimeoutException

import com.wavesplatform.dex.domain.utils.ScorexLogging

import scala.concurrent.duration.Deadline
import scala.concurrent.{ExecutionContext, Future}

class RepeatableRequests(queue: MatcherQueue, deadline: Deadline)(implicit ec: ExecutionContext) extends ScorexLogging {
  def firstOffset: Future[QueueEventWithMeta.Offset] = getOffset("first", deadline, queue.firstEventOffset)
  def lastOffset: Future[QueueEventWithMeta.Offset]  = getOffset("last", deadline, queue.lastEventOffset)

  private def getOffset[T](which: String, deadline: Deadline, get: => Future[T]): Future[T] = get.recoverWith {
    case e: TimeoutException =>
      log.warn(s"During receiving $which offset", e)
      if (deadline.isOverdue()) Future.failed(new TimeoutException(s"Can't get the $which offset from queue"))
      else getOffset(which, deadline, get)

    case e: Throwable =>
      log.error(s"Can't catch ${e.getClass.getName}", e)
      throw e
  }
}
