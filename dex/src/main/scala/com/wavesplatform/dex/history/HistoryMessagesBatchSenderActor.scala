package com.wavesplatform.dex.history

import akka.actor.{Actor, Cancellable}
import com.wavesplatform.dex.history.HistoryRouterActor.{HistoryMsg, StopAccumulate}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.reflect.ClassTag

abstract class HistoryMessagesBatchSenderActor[M <: HistoryMsg: ClassTag] extends Actor {

  val batchLinger: Long
  val batchEntries: Long

  def createAndSendBatch(batchBuffer: Iterable[M]): Unit

  private val batchBuffer: mutable.Set[M] = mutable.Set.empty[M]

  private def scheduleStopAccumulating: Cancellable =
    // Zero batch linger means that batch is restricted only by entries
    if (batchLinger == 0) Cancellable.alreadyCancelled else context.system.scheduler.scheduleOnce(batchLinger.millis, self, StopAccumulate)

  private def sendBatch(): Unit =
    if (batchBuffer.nonEmpty) {
      createAndSendBatch(batchBuffer)
      batchBuffer.clear()
    }

  def receive: Receive = awaitingHistoryMessages

  private def awaitingHistoryMessages: Receive = {
    case msg: M =>
      batchBuffer += msg
      // if batchEntries == 1, we send batch immediately without scheduling;
      // zero batch entries means that batch is restricted only by batch linger
      if (batchBuffer.size == batchEntries) sendBatch()
      else context become accumulateBuffer(scheduleStopAccumulating)
  }

  private def accumulateBuffer(scheduledStop: Cancellable): Receive = {
    case msg: M =>
      batchBuffer += msg
      if (batchBuffer.size == batchEntries) {
        scheduledStop.cancel()
        sendBatch()
        context become awaitingHistoryMessages
      }

    case StopAccumulate => sendBatch(); context become awaitingHistoryMessages
  }

}
