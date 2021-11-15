package com.wavesplatform.dex.domain.utils

import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import org.slf4j.{Logger, LoggerFactory}

case class LoggerFacade(logger: Logger, prefix: String = "") {

  def trace(message: => String): Unit = if (logger.isTraceEnabled) logger.trace(s"{} $message", prefix)
  def debug(message: => String, arg: Any): Unit = if (logger.isDebugEnabled) logger.debug(s"{} $message", prefix, arg)
  def debug(message: => String): Unit = if (logger.isDebugEnabled) logger.debug(s"{} $message", prefix)
  def info(message: => String): Unit = if (logger.isInfoEnabled) logger.info(s"{} $message", prefix)
  def info(message: => String, arg: Any): Unit = if (logger.isInfoEnabled) logger.info(s"{} $message", prefix, arg)
  def info(message: => String, throwable: Throwable): Unit = if (logger.isInfoEnabled) logger.info(s"$prefix $message", throwable)
  def warn(message: => String): Unit = if (logger.isWarnEnabled) logger.warn(s"{} $message", prefix)
  def warn(message: => String, throwable: Throwable): Unit = if (logger.isWarnEnabled) logger.warn(s"$prefix $message", throwable)
  def error(message: => String): Unit = if (logger.isErrorEnabled) logger.error(s"{} $message", prefix)
  def error(message: => String, throwable: Throwable): Unit = if (logger.isErrorEnabled) logger.error(s"$prefix $message", throwable)
}

trait ScorexLogging {

  protected lazy val log: LoggerFacade = LoggerFacade(LoggerFactory.getLogger(this.getClass))

  implicit class TaskExt[A](t: Task[A]) {

    def runAsyncLogErr(log: LoggerFacade = log)(implicit s: Scheduler): CancelableFuture[A] = logErr(log).runToFuture(s)

    def logErr(log: LoggerFacade = log): Task[A] = t.onErrorHandleWith { ex =>
      log.error(s"Error executing task", ex)
      Task.raiseError[A](ex)
    }

  }

}
