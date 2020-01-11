package com.wavesplatform.dex.it.docker

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import mouse.any._
import org.testcontainers.containers.GenericContainer
import org.testcontainers.images.builder.Transferable
import org.testcontainers.utility.MountableFile

object Implicits {
  // Can't create an implicit for Java's MountableFile.type
  object MountableFileOps {
    def fromContent(content: String): MountableFile = MountableFile.forHostPath {
      Files.createTempFile("dex-it", "") unsafeTap { Files.write(_, content getBytes StandardCharsets.UTF_8) }
    }
  }

  final implicit class GenericContainerOps[T <: GenericContainer[T]](val self: GenericContainer[T]) extends AnyVal {
    /**
      * Works only during configuration
      */
    def withCopyFileToContainer(containerPath: String, content: String): Unit =
      self.withCopyFileToContainer(MountableFileOps.fromContent(content), containerPath)

    /**
      * Works when a container is working
      */
    def copyFileToContainer(containerPath: String, content: String): Unit =
      self.copyFileToContainer(Transferable.of(content.getBytes(StandardCharsets.UTF_8)), containerPath)
  }
}
