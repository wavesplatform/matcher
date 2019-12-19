package com.wavesplatform.dex.it.docker.base.info

import com.dimafeng.testcontainers.GenericContainer.DockerImage

trait BaseContainerInfo {

  val image: DockerImage
  val baseLocalConfDir: String
  val baseConfFileName: String
  val baseContainerPath: String
  val restApiPort: Int
  val exposedPorts: Seq[Int]
  val netAlias: String

  def getEnv(containerName: String): Map[String, String]
}
