package com.wavesplatform.dex.it.config

import com.typesafe.config.{Config, ConfigRenderOptions}

object Implicits {
  final implicit class ConfigOps(val self: Config) extends AnyVal {
    def rendered: String =
      self
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
  }
}
