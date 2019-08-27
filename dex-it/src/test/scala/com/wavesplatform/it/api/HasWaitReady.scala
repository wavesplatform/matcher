package com.wavesplatform.it.api

trait HasWaitReady[F[_]] {
  def waitReady: F[Unit]
}
