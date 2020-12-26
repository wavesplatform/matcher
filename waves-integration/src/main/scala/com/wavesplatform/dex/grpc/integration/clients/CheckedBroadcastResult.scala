package com.wavesplatform.dex.grpc.integration.clients

sealed trait CheckedBroadcastResult extends Product with Serializable

object CheckedBroadcastResult {
  case object Unconfirmed extends CheckedBroadcastResult
  case object Confirmed extends CheckedBroadcastResult
  case class Failed(message: String) extends CheckedBroadcastResult
}
