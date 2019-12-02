package com.wavesplatform.dex

package object util {
  def getSimpleName(x: Any): String = x.getClass.getName.replaceAll(".*?(\\w+)\\$?$", "$1")
}
