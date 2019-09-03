package com.wavesplatform.it.docker

import com.typesafe.config.Config
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.settings.WavesSettings

abstract class DockerContainer {
  def id: String
  def number: Int
  def name: String
  def config: Config
  def basePath: String
}

class WavesNodeContainer(override val id: String,
                         override val number: Int,
                         override val name: String,
                         override val config: Config,
                         override val basePath: String)
    extends DockerContainer {
  val settings: WavesSettings = WavesSettings.fromRootConfig(config.resolve())

  override def toString: String = s"WavesNodeContainer(name=$name, id=$id)"
}

class DexContainer(override val id: String,
                   override val number: Int,
                   override val name: String,
                   override val config: Config,
                   override val basePath: String)
    extends DockerContainer {
  val settings: MatcherSettings = MatcherSettings.valueReader.read(config.resolve(), "waves.dex")

  override def toString: String = s"DexContainer(name=$name, id=$id)"
}
