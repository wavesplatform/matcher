package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files

import scala.io.Source

object RequestDeleter {

  def delRequests(file: File, deletedCount: Int): Unit = {
    if (Files.exists(file.toPath)) {
      val source = Source.fromFile(file)
      val output = new PrintWriter(s"requests-after-drop-${System.currentTimeMillis}.txt", "utf-8")

      var i = 0
      var j = 0
      var r = 0

      try {
        source
          .getLines()
          .map(line => {
            if (r < deletedCount)
              i = i + 1
            if (line.isEmpty || line.indexOf("{") == 0) {
              j = j + 1
              if (j % 3 == 0) {
                j = 0
                r = r + 1
              }
            }
            line
          })
          .drop(i)
          .foreach(l => output.println(l))
        println(s"$deletedCount of $r requests have been dropped from $file, and saved to $output")
      } finally output.close()
    }
  }
}
