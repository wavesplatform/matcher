package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.waves.WavesBlockchainGrpcContext
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.{Docker, MatcherSuiteBase}

class GrpcTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Seq(
    ConfigFactory.parseString("""|
      |waves {
      |  network.node-name = node02
      |  extensions = [ "com.wavesplatform.dex.GrpcServerExtension" ]
      |
      |  grpc {
      |    host = 0.0.0.0
      |    port = 6870
      |  }
      |}""".stripMargin).withFallback(Default.head)
  )

  private def dockerNode: Docker.DockerNode = dockerNodes().head

  private lazy val context = WavesBlockchainGrpcContext(
    matcherAddress = IssueEthTx.sender.toAddress,
    host = dockerNode.networkAddress.getHostString,
    port = dockerNode.nodeExternalPort(6870)
  )

  "wasForged" - {
    val id = IssueEthTx.id()

    "false for unknown tx" in {
      context.wasForged(id) shouldBe false
    }

    "true for forged tx" in {
      node.signedBroadcast(IssueEthTx.json())

      node.waitForTransaction(id.toString)
      context.wasForged(id) shouldBe true
    }
  }

  "broadcastTx - sunny day" in {
    context.broadcastTx(IssueWctTx)
    node.waitForTransaction(IssueWctTx.id().toString)
  }

}
