package com.wavesplatform.dex.api.websockets.statuses

import java.util.UUID

trait TerminationStatus {
  val connectionId: UUID
}

object TerminationStatus {
  final case class PongTimeout(connectionId: UUID)         extends TerminationStatus
  final case class MaxLifetimeExceeded(connectionId: UUID) extends TerminationStatus
}
