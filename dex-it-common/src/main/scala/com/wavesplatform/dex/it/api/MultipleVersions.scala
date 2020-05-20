package com.wavesplatform.dex.it.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.dex.it.dex.HasDex
import com.wavesplatform.dex.it.docker.{DexContainer, WavesNodeContainer}

trait MultipleVersions extends HasDex with HasWavesNode { self: BaseContainersKit =>
  private val dex2Tag  = Option(System.getenv("DEX_MULTIPLE_VERSIONS_PREVIOUS_TAG")).getOrElse("latest")
  private val node2Tag = Option(System.getenv("NODE_MULTIPLE_VERSIONS_PREVIOUS_TAG")).getOrElse("latest")

  protected lazy val wavesNode2: WavesNodeContainer = createWavesNode("waves-2", tag = node2Tag, netAlias = None)

  protected def dex2SuiteConfig: Config = dexInitialSuiteConfig.withFallback {
    ConfigFactory.parseString(
      s"""waves.dex {
         |  waves-blockchain-client.grpc.target = "${wavesNode2.networkAddress.getHostName}:6887"
         |}""".stripMargin
    )
  }

  protected lazy val dex2: DexContainer = createDex("dex-2", suiteInitialConfig = dex2SuiteConfig, tag = dex2Tag)
}
