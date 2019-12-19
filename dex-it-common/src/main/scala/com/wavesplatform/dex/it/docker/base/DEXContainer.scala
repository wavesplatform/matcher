package com.wavesplatform.dex.it.docker.base

import java.net.InetSocketAddress

import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.it.docker.base.info.DEXContainerInfo

final case class DEXContainer(name: String, underlying: GenericContainer) extends BaseContainer(underlying) {

  override def restApiAddress: InetSocketAddress = getExternalAddress(DEXContainerInfo.restApiPort)

  override def replaceSuiteConfig(newSuiteConfig: Config): Unit = replaceInternalConfig(DEXContainerInfo, this)("suite.conf", newSuiteConfig)

  override def getBasePath: String = DEXContainerInfo.baseContainerPath
}
