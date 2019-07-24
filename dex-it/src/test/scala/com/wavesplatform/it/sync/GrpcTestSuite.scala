package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.grpc.{TransactionsApiGrpc, TransactionsByIdRequest}
import com.wavesplatform.dex.waves.WavesBlockchainGrpcContext
import com.wavesplatform.it.{Docker, MatcherSuiteBase}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig.IssueEthTx
import io.grpc.ManagedChannelBuilder
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._

class GrpcTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Seq(
    ConfigFactory.parseString("""|
      |waves {
      |  network.node-name = node02
      |  extensions = [ "com.wavesplatform.api.grpc.GRPCServerExtension" ]
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

  "wasForged" in {
    node.signedBroadcast(IssueEthTx.json())
    val id = IssueEthTx.id()
    node.waitForTransaction(id.toString)

    val addr = IssueEthTx.sender.toAddress.bytes.arr
    println(s"addr: length=${addr.length}, content=${addr.mkString(", ")}| base64=${IssueEthTx.sender.toAddress.bytes.base64Raw}")
    println(s"id: length=${id.length}, content=${id.arr.mkString(", ")}| base64=${id.base64}")
    context.wasForged(id) shouldBe true
  }

}
