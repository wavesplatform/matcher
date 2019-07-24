package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.grpc.{TransactionsApiGrpc, TransactionsByIdRequest}
import com.wavesplatform.it.{Docker, MatcherSuiteBase}
import com.wavesplatform.it.NodeConfigs.Default
import io.grpc.ManagedChannelBuilder

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
  private lazy val channel = ManagedChannelBuilder
    .forAddress(
      dockerNode.networkAddress.getHostString,
      dockerNode.nodeExternalPort(6870)
    )
    .usePlaintext()
    .build

  private lazy val transactions = TransactionsApiGrpc.blockingStub(channel)

  "transactions" in {
    val txs = transactions.getStatuses(TransactionsByIdRequest())
    println(s"txs: ${txs.mkString(", ")}")
  }

}
