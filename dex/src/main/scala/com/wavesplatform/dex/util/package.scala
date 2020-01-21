package com.wavesplatform.dex

package object util {

  def getSimpleName(x: Any): String                             = x.getClass.getName.replaceAll(".*?(\\w+)\\$?$", "$1")
  def forceStopApplication(reason: ApplicationStopReason): Unit = System.exit(reason.code)

}
