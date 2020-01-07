package com.wavesplatform.dex.time

import java.net.{InetAddress, SocketTimeoutException}

import com.wavesplatform.dex.domain.utils.ScorexLogging
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.execution.{ExecutionModel, Scheduler}
import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.duration.DurationInt

class NTP(ntpServer: String) extends Time with ScorexLogging with AutoCloseable {

  private val offsetPanicThreshold = 1000000L
  private val ExpirationTimeout    = 60.seconds
  private val RetryDelay           = 10.seconds
  private val ResponseTimeout      = 10.seconds

  private implicit val scheduler: SchedulerService = Scheduler.singleThread(name = "time-impl", executionModel = ExecutionModel.AlwaysAsyncExecution)

  private val client = new NTPUDPClient()
  client.setDefaultTimeout(ResponseTimeout.toMillis.toInt)

  @volatile private var offset = 0L
  private val updateTask: Task[Unit] = {
    def newOffsetTask: Task[Option[(InetAddress, java.lang.Long)]] = Task {
      try {
        client.open()
        val info = client.getTime(InetAddress.getByName(ntpServer))
        info.computeDetails()
        Option(info.getOffset).map { offset =>
          val r = if (Math.abs(offset) > offsetPanicThreshold) throw new Exception("Offset is suspiciously large") else offset
          (info.getAddress, r)
        }
      } catch {
        case _: SocketTimeoutException =>
          None
        case t: Throwable =>
          log.warn("Problems with NTP: ", t)
          None
      } finally {
        client.close()
      }
    }

    newOffsetTask.flatMap {
      case None if !scheduler.isShutdown => updateTask.delayExecution(RetryDelay)
      case Some((server, newOffset)) if !scheduler.isShutdown =>
        log.trace(s"Adjusting time with $newOffset milliseconds, source: ${server.getHostAddress}.")
        offset = newOffset
        updateTask.delayExecution(ExpirationTimeout)
      case _ => Task.unit
    }
  }

  def correctedTime(): Long = System.currentTimeMillis() + offset

  private var txTime: Long = 0

  def getTimestamp(): Long = {
    txTime = Math.max(correctedTime(), txTime + 1)
    txTime
  }

  private val taskHandle = updateTask.runAsync {
    case Left(e) => log.error(s"Error executing task", e)
    case _       =>
  }

  override def close(): Unit = {
    log.info("Shutting down Time")
    taskHandle.cancel()
    scheduler.shutdown()
  }
}
