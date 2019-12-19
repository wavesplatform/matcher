package com.wavesplatform.dex.it.docker.base

import java.net.InetSocketAddress

import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.it.docker.base.info.WavesNodeContainerInfo

final case class WavesNodeContainer(name: String, underlying: GenericContainer) extends BaseContainer(underlying) {

  override def restApiAddress: InetSocketAddress = getExternalAddress(WavesNodeContainerInfo.restApiPort)
  def networkAddress: InetSocketAddress          = getInternalAddress(WavesNodeContainerInfo.networkPort)

  override def replaceSuiteConfig(newSuiteConfig: Config): Unit = {
    replaceInternalConfig(WavesNodeContainerInfo, underlying)("suite.conf", newSuiteConfig)
  }

  override def getBasePath: String = WavesNodeContainerInfo.baseContainerPath
}
