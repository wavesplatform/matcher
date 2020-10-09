package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files

import scala.io.Source

object RequestDeleter {

  def delRequests(file: File, deletedCount: Int): Unit =
    if (Files.exists(file.toPath)) {
      val source = Source.fromFile(file)
      val outputFile = s"requests-after-drop-${System.currentTimeMillis}.txt"
      val output = new PrintWriter(outputFile, "utf-8")

      var i = 0
      var j = 0
      var r = 0

      try {
        source
          .getLines()
          .map { line =>
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
          }
          .drop(i)
          .foreach(line => output.print(s"$line\r\n"))
        println(s"$deletedCount of $r requests have been dropped from ${file.getAbsolutePath}, and saved to $outputFile")
      } finally output.close()
    }

}
