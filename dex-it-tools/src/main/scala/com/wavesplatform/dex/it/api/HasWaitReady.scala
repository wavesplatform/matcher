package com.wavesplatform.dex.it.api

trait HasWaitReady[F[_]] {
  def waitReady: F[Unit]
}
