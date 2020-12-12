package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent

trait BlockchainUpdatesControlledStream extends ControlledStream[SubscribeEvent] {
  def startFrom(height: Int): Unit
  def requestNext(): Unit
}
