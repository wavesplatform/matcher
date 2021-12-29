package com.wavesplatform.dex

import cats.syntax.either._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.settings.ConfigOps.ConfigOps
import pureconfig.ConfigSource

import scala.util.Try

package object settings {


  implicit def toConfigOps(config: Config): ConfigOps = new ConfigOps(config)


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
