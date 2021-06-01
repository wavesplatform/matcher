package com.wavesplatform.dex.it.api

import com.wavesplatform.dex.it.docker.{ConfigurableToxicProxyContainer, PortBindingKeeper, WavesNodeContainer}
import com.wavesplatform.dex.it.docker.ConfigurableToxicProxyContainer.ContainerProxy

import scala.jdk.CollectionConverters._

trait HasToxiProxy { self: BaseContainersKit =>

  protected val toxiProxyHostName = s"$networkName-toxiproxy"
  // Two ports for two extensions: blockchain updates and ours
  private val exposedPorts = Seq(WavesNodeContainer.matcherGrpcExtensionPort, WavesNodeContainer.blockchainUpdatesGrpcExtensionPort)

  protected val toxiContainer: ConfigurableToxicProxyContainer = mkToxiProxyContainer

  private def mkToxiProxyContainer = {
    val cfgContainer = new ConfigurableToxicProxyContainer("shopify/toxiproxy:2.1.0", exposedPorts.size)
    cfgContainer.container.withNetwork(network)
    cfgContainer.container.withNetworkAliases(toxiProxyHostName)
    cfgContainer.container.withCreateContainerCmdModifier { cmd =>
      cmd.withName(toxiProxyHostName)
      cmd.withIpv4Address(getIp(13))
      cmd.getHostConfig.withPortBindings(PortBindingKeeper.getBindings(cmd, exposedPorts))
    }
    cfgContainer
  }

  protected def getInnerToxiProxyPort(proxy: ContainerProxy): Int =
    toxiContainer.getContainerInfo.getNetworkSettings.getPorts.getBindings.asScala
      .find { case (_, bindings) => bindings.head.getHostPortSpec == proxy.proxyPort.toString }
      .map(_._1.getPort)
      .getOrElse(throw new IllegalStateException(s"There is no inner port for proxied one: ${proxy.proxyPort}"))

  protected def mkToxiProxy(hostname: String, port: Int): ContainerProxy = toxiContainer.getProxy(hostname, port)

  toxiContainer.start()
  logExposedPortsInfo("At start for toxi proxi")

  private def logExposedPortsInfo(prefix: String): Unit = {
    val networkSettings = toxiContainer.getDockerClient
      .inspectContainerCmd(toxiContainer.getContainerId)
      .exec()
      .getNetworkSettings
    val portBindingsStr = networkSettings
      .getPorts
      .getBindings
      .asScala
      .flatMap { case (exposedPort, portBindings) =>
        portBindings.headOption.map { containerPort =>
          s"${exposedPort.getPort} -> ${containerPort.getHostPortSpec}/${exposedPort.getProtocol}"
        }
      }
      .mkString("; ")
    log.info(s"$prefix Exposed ports for networks ${networkSettings.getNetworks.keySet()}: $portBindingsStr")
  }

}
