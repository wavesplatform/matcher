package com.wavesplatform.dex.settings.utils

import java.util.Properties
import scala.jdk.CollectionConverters.MapHasAsScala

import cats.data.Validated
import com.typesafe.config.{Config, ConfigRenderOptions, ConfigValueType}
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

    def filterKeys(part: String): Config = {
      var oc = config

      def filter(c: Config, b: String = ""): Unit = {

        c.root().asScala.foreach(obj => {
          val k = obj._1
          val v = obj._2
          val p = if (b.nonEmpty) s"$b.$k" else k

          if (k.contains(part)) oc = oc.withoutPath(p)
          else if (v.valueType().equals(ConfigValueType.OBJECT)) {
            c.getObject(k).isEmpty match {
              case true => oc = oc.withoutPath(p)
              case _ => filter(c.getObject(k).toConfig, p)
            }
          }
        })
      }

      filter(oc)

      oc
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
