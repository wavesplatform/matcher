package com.wavesplatform.dex

import java.io.{PrintWriter, StringWriter}

package object error {
  def formatStackTrace(e: Throwable): String = {
    val r = new StringWriter()
    e.printStackTrace(new PrintWriter(r))
    r.flush()
    r.getBuffer.toString
  }
}
