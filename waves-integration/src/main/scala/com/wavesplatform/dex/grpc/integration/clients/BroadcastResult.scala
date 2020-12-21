package com.wavesplatform.dex.grpc.integration.clients

sealed trait BroadcastResult extends Product with Serializable

object BroadcastResult {
  case object Added extends BroadcastResult
  case object NotAdded extends BroadcastResult
  case class Failed(message: String) extends BroadcastResult
}
