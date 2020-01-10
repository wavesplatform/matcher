package com.wavesplatform.dex.it.docker.base.info

import com.dimafeng.testcontainers.GenericContainer.DockerImage
import com.wavesplatform.dex.it.docker.base.BaseContainer

object DexContainerInfo extends BaseContainerInfo {

  override val image: DockerImage = "com.wavesplatform/dex-it:latest"

  override val baseLocalConfDir: String  = "dex-servers"
  override val baseConfFileName: String  = "dex-base.conf"
  override val baseContainerPath: String = "/opt/waves-dex"

  override val restApiPort: Int       = 6886 // application.conf waves.dex.rest-api.port
  override val exposedPorts: Seq[Int] = Seq(restApiPort)
  override val netAlias: String       = "d3x"

  // file name, content, to log content
  override val specificFiles: Seq[(String, String, Boolean)] = Seq(
    ("/doc/logback-container.xml", BaseContainer.getRawContentFromResource("logback-container.xml"), false)
  )

  override def getEnv(containerName: String): Map[String, String] = Map(
    "WAVES_DEX_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_DEX_OPTS" -> Seq(
      "-J-Xmx1024M",
      "-Dlogback.stdout.enabled=false",
      "-Dlogback.file.enabled=false",
      s"-Dlogback.configurationFile=$baseContainerPath/doc/logback.xml",
      s"-Dlogback.include.file=$baseContainerPath/doc/logback-container.xml"
    ).mkString(" ", " ", " ")
  )
}
