package com.wavesplatform.dex.it.docker.base.info

import com.dimafeng.testcontainers.GenericContainer.DockerImage
import com.wavesplatform.dex.it.docker.base.BaseContainer

object DexContainerInfo extends BaseContainerInfo {

  val image: DockerImage = "com.wavesplatform/dex-it:latest"

  val baseLocalConfDir: String  = "dex-servers"
  val baseConfFileName: String  = "dex-base.conf"
  val baseContainerPath: String = "/opt/waves-dex"

  val restApiPort: Int       = 6886 // application.conf waves.dex.rest-api.port
  val exposedPorts: Seq[Int] = Seq(restApiPort)
  val netAlias: String       = "d3x"

  // file name, content, to log content
  override val specificFiles: Seq[(String, String, Boolean)] = Seq(
    ("/doc/logback-container.xml", BaseContainer.getRawContentFromResource("logback-container.xml"), false)
  )

  def getEnv(containerName: String): Map[String, String] = Map(
    "WAVES_DEX_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_DEX_OPTS" -> Seq(
      "-Dlogback.stdout.enabled=false",
      "-Dlogback.file.enabled=false",
      s"-Dlogback.configurationFile=$baseContainerPath/doc/logback.xml",
      s"-Dlogback.include.file=$baseContainerPath/doc/logback-container.xml"
    ).mkString(" ", " ", " ")
  )
}
