package com.wavesplatform.dex.doc

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import com.wavesplatform.utils.ScorexLogging

object DocGeneratorApp extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val outDir  = args(0)
    val outPath = Paths.get(outDir)
    Files.createDirectories(outPath)
    val errors = new PrintWriter(outPath.resolve("errors.md").toFile)
    try {
      errors.write(MatcherErrorDoc.mkMarkdown)
    } finally {
      errors.close()
    }

    log.info("Completed")
  }
}
