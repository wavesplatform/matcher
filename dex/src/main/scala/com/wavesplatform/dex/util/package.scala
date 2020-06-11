package com.wavesplatform.dex

package object util {
  def forceStopApplication(reason: ApplicationStopReason): Unit = System.exit(reason.code)
}
