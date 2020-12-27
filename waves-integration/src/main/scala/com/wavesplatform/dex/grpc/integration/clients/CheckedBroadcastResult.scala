package com.wavesplatform.dex.grpc.integration.clients

sealed trait CheckedBroadcastResult extends Product with Serializable

object CheckedBroadcastResult {
  case class Unconfirmed(isNew: Boolean) extends CheckedBroadcastResult
  case object Confirmed extends CheckedBroadcastResult
  case class Failed(message: String, canRetry: Boolean) extends CheckedBroadcastResult
}
