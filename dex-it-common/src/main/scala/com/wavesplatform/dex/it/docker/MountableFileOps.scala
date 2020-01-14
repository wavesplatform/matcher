package com.wavesplatform.dex.it.docker

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import mouse.any._
import org.testcontainers.utility.MountableFile

// Can't create an implicit for Java's MountableFile.type
object MountableFileOps {
  def fromContent(content: String): MountableFile = MountableFile.forHostPath {
    Files.createTempFile("dex-it", "") unsafeTap { Files.write(_, content getBytes StandardCharsets.UTF_8) }
  }
}
