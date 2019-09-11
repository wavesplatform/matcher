package com.wavesplatform.it.docker

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.utils.ScorexLogging

trait DockerExtensions extends ScorexLogging {
  protected def dockerClient: Docker

  protected def restartContainer(container: DockerContainer, api: HasWaitReady[cats.Id]): Unit = {
    dockerClient.stop(container)
    dockerClient.start(container)
    api.waitReady
  }

  protected def replaceSuiteConfig(container: DockerContainer, config: Config): Unit =
    replaceSuiteConfig(
      container,
      config
        .resolve()
        .root()
        .render(
          ConfigRenderOptions
            .concise()
            .setOriginComments(false)
            .setComments(false)
            .setFormatted(true)
            .setJson(false)
        )
    )

  protected def replaceSuiteConfig(container: DockerContainer, content: String): Unit = {
    val path = Paths.get(container.basePath, "suite.conf")
    log.trace(s"Replacing '$path' of $container by:\n$content")
    dockerClient.writeFile(container, path, content)
  }
}
