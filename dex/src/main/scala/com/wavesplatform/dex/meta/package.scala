package com.wavesplatform.dex

package object meta {
  def getSimpleName(x: Any): String = x.getClass.getName.replaceAll(".*?(\\w+)\\$?$", "$1")
}
