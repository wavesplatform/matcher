package com.wavesplatform.dex.it.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.dex.it.dex.HasDex
import com.wavesplatform.dex.it.docker.{DexContainer, WavesNodeContainer}

trait MultipleVersions extends HasDex with HasWavesNode { self: BaseContainersKit =>
  private lazy val dex2Image = Option(System.getenv("OTHER_DEX_IMAGE"))
    .getOrElse(throw new RuntimeException("Please specify the OTHER_DEX_IMAGE environment variable"))

  private lazy val node2Image = Option(System.getenv("OTHER_NODE_IMAGE"))
    .getOrElse(throw new RuntimeException("Please specify the OTHER_NODE_IMAGE environment variable"))

  protected lazy val wavesNode2: WavesNodeContainer = createWavesNode("waves-2", image = node2Image, netAlias = None)

  protected def dex2SuiteConfig: Config = dexInitialSuiteConfig.withFallback {
    ConfigFactory.parseString(
      s"""waves.dex {
         |  waves-blockchain-client.grpc.target = "${wavesNode2.networkAddress.getHostName}:6887"
         |}""".stripMargin
    )
  }

  protected lazy val dex2: DexContainer = createDex("dex-2", suiteInitialConfig = dex2SuiteConfig, image = dex2Image)
}
