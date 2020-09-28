package com.wavesplatform.dex

package object app {
  def forceStopApplication(reason: ApplicationStopReason): Unit = System.exit(reason.code)
}
