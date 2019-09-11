package com.wavesplatform.dex.it.api

import com.typesafe.config.Config

trait HasWaitReady[F[_]] {
  def waitReady: F[Unit]
  def config: F[Config]
}
