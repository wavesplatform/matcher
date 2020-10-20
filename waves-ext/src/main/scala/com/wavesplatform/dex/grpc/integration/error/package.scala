package com.wavesplatform.dex.grpc.integration

import java.io.{PrintWriter, StringWriter}

package object error {

  def formatStackTrace(e: Throwable): String = {
    val stringWriter = new StringWriter()
    e.printStackTrace(new PrintWriter(stringWriter))
    stringWriter.flush()
    stringWriter.getBuffer.toString
  }

}
