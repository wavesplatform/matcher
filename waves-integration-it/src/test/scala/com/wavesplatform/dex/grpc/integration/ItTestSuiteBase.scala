package com.wavesplatform.dex.grpc.integration

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.grpc.integration.ItTestSuiteBase._
import com.wavesplatform.dex.grpc.integration.config.SetupAddressScheme
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{Docker, ReportingTestName}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext

trait ItTestSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with ReportingTestName with NodesFromDocker {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/waves-integration-it:latest",
    tag = getClass.getSimpleName
  )

  protected def node: Docker.DockerNode  = dockerNodes().head
  protected def nodeConfigs: Seq[Config] = Seq(baseConfig)
}

object ItTestSuiteBase {
  private val baseConfig = ConfigFactory.parseResources("nodes/waves-01.conf")
  SetupAddressScheme.setup()
}
