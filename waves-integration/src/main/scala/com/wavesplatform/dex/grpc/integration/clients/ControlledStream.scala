package com.wavesplatform.dex.grpc.integration.clients

import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import monix.reactive.Observable

trait ControlledStream[T] extends AutoCloseable {
  val stream: Observable[T]
  val systemStream: Observable[SystemEvent]
  def stop(): Unit
  def close(): Unit
}

object ControlledStream {
  sealed trait SystemEvent extends Product with Serializable

  object SystemEvent {
    case object BecameReady extends SystemEvent

    /**
     * Manually or by an error
     */
    case object Stopped extends SystemEvent

    /**
     * During shutdown
     */
    case object Closed extends SystemEvent
  }

}
