package com.wavesplatform.dex.queue

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait MatcherQueue {
  def startConsume(fromOffset: ValidatedCommandWithMeta.Offset, process: List[ValidatedCommandWithMeta] => Future[Unit]): Unit

  /**
   * @return Depending on settings and result:
   *         Future.successful(None)    if storing is disabled
   *         Future.successful(Some(x)) if storing is enabled and it was successful
   *         Future.failed(error)       if storing is enabled and it was failed
   */
  def store(payload: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]]

  /**
   * @return -1 if topic is empty or even it doesn't exist
   */
  def firstOffset: Future[ValidatedCommandWithMeta.Offset]

  /**
   * @return -1 if topic is empty or even it doesn't exist
   */
  def lastOffset: Future[ValidatedCommandWithMeta.Offset]

  def close(timeout: FiniteDuration): Future[Unit]
}

object MatcherQueue {
  type StoreValidatedCommand = ValidatedCommand => Future[Option[ValidatedCommandWithMeta]]

  private val stored: Future[Option[ValidatedCommandWithMeta]] = Future.successful(None)

  private[queue] trait Producer {
    def store(command: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]]
    def close(timeout: FiniteDuration): Unit
  }

  private[queue] object IgnoreProducer extends Producer {
    override def store(command: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]] = stored
    override def close(timeout: FiniteDuration): Unit = {}
  }

}
