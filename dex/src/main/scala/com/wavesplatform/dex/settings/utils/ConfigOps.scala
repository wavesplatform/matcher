package com.wavesplatform.dex.settings.utils

import java.util.Properties

import com.typesafe.config.{Config, ConfigRenderOptions}
import mouse.any._

object ConfigOps {

  implicit final class ConfigOps(val config: Config) extends AnyVal {

    def toProperties: Properties = new Properties() unsafeTap { properties =>
      config.entrySet().forEach { entry =>
        properties.setProperty(entry.getKey, config getString entry.getKey)
      }
    }

    def rendered: String =
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

  }

}
