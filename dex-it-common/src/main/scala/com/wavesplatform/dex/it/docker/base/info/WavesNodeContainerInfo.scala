package com.wavesplatform.dex.it.docker.base.info

import com.dimafeng.testcontainers.GenericContainer.DockerImage
import com.wavesplatform.dex.it.docker.base.BaseContainer

object WavesNodeContainerInfo extends BaseContainerInfo {

  override val image: DockerImage = "com.wavesplatform/waves-integration-it:latest"

  override val baseLocalConfDir: String  = "nodes"
  override val baseConfFileName: String  = "waves-base.conf"
  override val baseContainerPath: String = "/opt/waves"

  override val restApiPort: Int = 6869 // application.conf waves.rest-api.port
  val networkPort: Int          = 6863 // application.conf waves.network.port
  val dexGrpcExtensionPort: Int = 6887 // application.conf waves.dex.grpc.integration.port

  override val exposedPorts: Seq[Int] = Seq(restApiPort, networkPort, dexGrpcExtensionPort)
  override val netAlias: String       = "waves.nodes"

  override val specificFiles: Seq[(String, String, Boolean)] = Seq(
    ("/logback-container.xml", BaseContainer.getRawContentFromResource("logback-container.xml"), false)
  )

  override def getEnv(containerName: String): Map[String, String] = Map(
    "WAVES_NODE_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_OPTS" -> List(
      "-Xmx1024M",
      s"-Dlogback.configurationFile=$baseContainerPath/logback-container.xml"
    ).mkString(" ", " ", " ")
  )
}
