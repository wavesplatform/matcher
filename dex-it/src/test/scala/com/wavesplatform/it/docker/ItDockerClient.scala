package com.wavesplatform.it.docker

import com.typesafe.config.Config
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.settings.WavesSettings

abstract class DockerContainer {
  def id: String
  def number: Int
  def name: String
  def config: Config
}

class WavesNodeContainer(override val id: String, override val number: Int, override val name: String, override val config: Config)
    extends DockerContainer {
  val settings: WavesSettings = WavesSettings.fromRootConfig(config.resolve())
}

class DexContainer(override val id: String, override val number: Int, override val name: String, override val config: Config)
    extends DockerContainer {
  val settings: MatcherSettings = MatcherSettings.valueReader.read(config.resolve(), "waves.dex")
}
