package com.wavesplatform.dex.it.docker

import org.testcontainers.utility.MountableFile

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.util.chaining._

// Can't create an implicit for Java's MountableFile.type
object MountableFileOps {

  def fromContent(content: String): MountableFile = MountableFile.forHostPath {
    Files.createTempFile("dex-it", "") tap (Files.write(_, content getBytes StandardCharsets.UTF_8))
  }

}
