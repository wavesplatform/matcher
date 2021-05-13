package com.wavesplatform.dex.logs

import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.dex.domain.utils.ScorexLogging

object SystemInformationReporter extends ScorexLogging {

  def report(config: Config): Unit = {
    val renderOptions = ConfigRenderOptions
      .defaults()
      .setOriginComments(false)
      .setComments(false)
      .setFormatted(false)

    val logInfo: Seq[(String, Any)] = Seq(
      "Available processors" -> Runtime.getRuntime.availableProcessors,
      "Max memory available" -> Runtime.getRuntime.maxMemory
    ) ++ Seq(
      "os.name",
      "os.version",
      "os.arch",
      "java.version",
      "java.vendor",
      "java.home",
      "java.class.path",
      "user.dir",
      "sun.net.inetaddr.ttl",
      "sun.net.inetaddr.negative.ttl",
      "networkaddress.cache.ttl",
      "networkaddress.cache.negative.ttl"
    ).map { x =>
      x -> System.getProperty(x)
    } ++ Seq(
      "Configuration" -> config.root.render(renderOptions)
    )

    log.debug(logInfo.map { case (n, v) => s"$n: $v" }.mkString("\n"))
  }

}
