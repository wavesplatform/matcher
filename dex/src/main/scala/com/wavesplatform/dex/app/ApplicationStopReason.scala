package com.wavesplatform.dex.app

import scala.util.control.NoStackTrace

sealed abstract class ApplicationStopReason(val code: Int) extends RuntimeException with NoStackTrace

case object StartingMatcherError extends ApplicationStopReason(10) {
  override val getMessage: String = "Can't start matcher, see log"
}

case class RecoveryError(detailed: String) extends ApplicationStopReason(12) {
  override val getMessage: String = s"A recovery error: $detailed"
}

case object EventProcessingError extends ApplicationStopReason(16) {
  override val getMessage: String = "An error during a queue messages processing"
}

case object NotSynchronizedNodeError extends ApplicationStopReason(18) {
  override val getMessage: String = "Try to sync up your node with the network"
}

case object MatcherStateCheckingFailedError extends ApplicationStopReason(20) {
  override val getMessage: String = "Matcher checking failed, see log"
}
