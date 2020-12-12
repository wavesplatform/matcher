package com.wavesplatform.dex.grpc.integration.clients.matcherext

import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.dex.grpc.integration.services.UtxEvent

trait UtxEventsControlledStream extends ControlledStream[UtxEvent] {
  def start(): Unit
}
