package com.wavesplatform.dex.settings.utils

import java.util.Properties

import com.typesafe.config.{Config, ConfigObject, ConfigRenderOptions}
import mouse.any._

import scala.jdk.CollectionConverters.MapHasAsScala

object ConfigOps {

  implicit final class ConfigOps(val config: Config) extends AnyVal {

    def toProperties: Properties = new Properties() unsafeTap { properties =>
      config.entrySet().forEach { entry =>
        properties.setProperty(entry.getKey, config getString entry.getKey)
      }
    }

    def withoutKeys(s: Set[String]): Config = {
      def withoutKeys(c: ConfigObject, s: Set[String]): ConfigObject =
        c.asScala.foldLeft(c) { case (r, (k, v)) =>
          if (s.exists(k.contains(_))) r.withoutKey(k)
          else v match {
            case v: ConfigObject => r.withValue(k, withoutKeys(v, s))
            case _ => r
          }
        }

      withoutKeys(config.root(), s).toConfig
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
