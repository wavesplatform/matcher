package com.wavesplatform.dex.it.docker.base.info

import com.dimafeng.testcontainers.GenericContainer.DockerImage

object WavesNodeContainerInfo extends BaseContainerInfo {

  val image: DockerImage = "com.wavesplatform/waves-integration-it:latest"

  val baseLocalConfDir: String  = "nodes"
  val baseConfFileName: String  = "waves-base.conf"
  val baseContainerPath: String = "/opt/waves"

  val restApiPort: Int          = 6869 // application.conf waves.rest-api.port
  val networkPort: Int          = 6863 // application.conf waves.network.port
  val dexGrpcExtensionPort: Int = 6887 // application.conf waves.dex.grpc.integration.port

  val exposedPorts: Seq[Int] = Seq(restApiPort, networkPort, dexGrpcExtensionPort)
  val netAlias: String       = "waves.nodes"

  def getEnv(containerName: String): Map[String, String] = Map(
    "WAVES_NODE_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_OPTS"            -> s" -Xmx1024M -Dlogback.configurationFile=$baseContainerPath/logback.xml"
  )
}
