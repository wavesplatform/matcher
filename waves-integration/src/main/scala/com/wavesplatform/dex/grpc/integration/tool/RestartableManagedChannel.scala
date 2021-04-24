package com.wavesplatform.dex.grpc.integration.tool

import io.grpc.ManagedChannel

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

final class RestartableManagedChannel(mkManagedChannel: () => ManagedChannel) {

  private var channel: ManagedChannel = _
  private var isClosed: Boolean = false

  def stop(): Unit = synchronized {
    checkIsClosed()
    if (channel != null) {
      channel.shutdown()
      channel = null
    }
  }

  def restart(): Unit = synchronized {
    checkIsClosed()
    if (channel != null)
      channel.shutdown()
    channel = mkManagedChannel()
  }

  def getChannel: ManagedChannel = synchronized {
    checkIsClosed()
    if (channel == null)
      channel = mkManagedChannel()
    channel
  }

  def shutdown(awaitTime: Duration): Unit = synchronized {
    mkClosed()
    if (channel != null) {
      channel.shutdown()
      channel.awaitTermination(awaitTime.toMillis, TimeUnit.MILLISECONDS)
      channel = null
    }
  }

  private def checkIsClosed(): Unit =
    if (isClosed)
      throw new RuntimeException("managed channel is closed")

  private def mkClosed(): Unit = {
    checkIsClosed()
    isClosed = true
  }

}
