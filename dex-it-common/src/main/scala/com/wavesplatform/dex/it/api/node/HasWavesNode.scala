package com.wavesplatform.dex.it.api.node

import cats.Functor
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.fp.CanExtract
import mouse.any._

trait HasWavesNode { self: BaseContainersKit =>

  protected implicit def toNodeExplicitGetOps[F[_]: Functor: CanExtract](self: NodeApi[F]): NodeApiOps.ExplicitGetNodeApiOps[F] = {
    new NodeApiOps.ExplicitGetNodeApiOps[F](self)
  }

  protected def wavesNodeInitialSuiteConfig: Config = ConfigFactory.empty()

  protected lazy val wavesNodeRunConfig: Config = GenesisConfig.config

  protected def createWavesNode(name: String,
                                runConfig: Config = wavesNodeRunConfig,
                                suiteInitialConfig: Config = wavesNodeInitialSuiteConfig): WavesNodeContainer =
    WavesNodeContainer(name, networkName, network, getIp(name), wavesNodeRunConfig, wavesNodeInitialSuiteConfig, localLogsDir) unsafeTap addKnownContainer

  lazy val wavesNode1: WavesNodeContainer = createWavesNode("waves-1")
}
