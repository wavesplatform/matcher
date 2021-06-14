package com.wavesplatform.dex

import cats.syntax.either._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import com.wavesplatform.dex.tool.LocaleUtils
import pureconfig.ConfigSource

import java.text.DecimalFormat
import scala.util.Try

package object settings {

  implicit def toConfigOps(config: Config): ConfigOps = new ConfigOps(config)

  private val format = new DecimalFormat("#.################", LocaleUtils.symbols)

  /** Formats amount or price */
  def formatValue(value: BigDecimal): String = format.format(value.bigDecimal)

  def loadMatcherSettings(config: Config): Either[String, MatcherSettings] =
    Try(ConfigSource.fromConfig(loadConfig(config)).at("waves.dex").loadOrThrow[MatcherSettings])
      .toEither.leftMap(th => s"Cannot load matcher settings: ${th.getWithStackTrace}")

  def loadConfig(config: Config): Config = loadConfig(Some(config))

  def loadConfig(maybeConfig: Option[Config]): Config = {

    val defaults = ConfigFactory.defaultOverrides()
    val external = maybeConfig.fold(defaults)(defaults.withFallback)

    external
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
  }

}
