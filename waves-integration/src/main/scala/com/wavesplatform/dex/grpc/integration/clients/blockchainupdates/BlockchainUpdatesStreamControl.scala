package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

trait BlockchainUpdatesStreamControl {
  def checkpoint(height: Int): Unit
  def requestNext(): Unit
  def restartFrom(height: Int): Unit
  def stop(): Unit
}
