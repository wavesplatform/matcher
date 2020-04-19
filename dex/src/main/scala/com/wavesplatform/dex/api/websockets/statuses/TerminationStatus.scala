package com.wavesplatform.dex.api.websockets.statuses

sealed trait TerminationStatus
object TerminationStatus {
  case object PongTimeout         extends TerminationStatus
  case object MaxLifetimeExceeded extends TerminationStatus
}
