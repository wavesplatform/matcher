package com.wavesplatform.dex.settings.utils

import java.util.Properties

import cats.data.Validated
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import mouse.any._
import net.ceedubs.ficus.readers.ValueReader

object ConfigOps {

  final implicit class ConfigOps(config: Config) {

    val cfgValidator: ConfigSettingsValidator = ConfigSettingsValidator(config)

    def getValidatedSet[T: ValueReader](path: String): Set[T] = {
      cfgValidator.validateList[T](path).map(_.toSet) getValueOrThrowErrors
    }

    def getValidatedMap[K, V: ValueReader](path: String)(keyValidator: String => Validated[String, K]): Map[K, V] = {
      cfgValidator.validateMap(path)(keyValidator) getValueOrThrowErrors
    }

    def getValidatedByPredicate[T: ValueReader](path: String)(predicate: T => Boolean, errorMsg: String): T = {
      cfgValidator.validateByPredicate(path)(predicate, errorMsg) getValueOrThrowErrors
    }

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
