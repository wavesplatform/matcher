package com.wavesplatform.dex.it.api

import org.testcontainers.containers.ToxiproxyContainer
import org.testcontainers.containers.ToxiproxyContainer.ContainerProxy

import scala.jdk.CollectionConverters._

trait HasToxiProxy { self: BaseContainersKit =>

  protected val toxiProxyHostName = s"$networkName-toxiproxy"

  private val container: ToxiproxyContainer = new ToxiproxyContainer()
    .withNetwork(network)
    .withNetworkAliases(toxiProxyHostName)
    .withExposedPorts(8666)
    .withCreateContainerCmdModifier { cmd =>
      cmd withName toxiProxyHostName
      cmd withIpv4Address getIp(13)
    }

  protected def getInnerToxiProxyPort(proxy: ContainerProxy): Int = {
    container.getContainerInfo.getNetworkSettings.getPorts.getBindings.asScala
      .find { case (_, bindings) => bindings.head.getHostPortSpec == proxy.getProxyPort.toString }
      .map(_._1.getPort)
      .getOrElse(throw new IllegalStateException(s"There is no inner port for proxied one: ${proxy.getProxyPort}"))
  }

  protected def mkToxiProxy(hostname: String, port: Int): ToxiproxyContainer.ContainerProxy = container.getProxy(hostname, port)

  container.start()
}
