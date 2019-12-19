package com.wavesplatform.dex.it.docker

import com.wavesplatform.dex.it.docker.base.BaseContainer
import org.testcontainers.containers.ToxiproxyContainer
import org.testcontainers.containers.ToxiproxyContainer.ContainerProxy
import scala.collection.JavaConverters._

class ToxiProxy {

  val name            = "toxiproxy"
  val containerNumber = 13
  val ip: String      = BaseContainer.getIp(containerNumber)

  val container: ToxiproxyContainer =
    new ToxiproxyContainer()
      .withNetwork(BaseContainer.network)
      .withNetworkAliases(name)
      .withCreateContainerCmdModifier { cmd =>
        cmd withName name
        cmd withIpv4Address ip
      }

  def start(): ToxiProxy = { container.start(); this }

  def getInnerProxyPort(proxy: ContainerProxy): Int = {
    container.getContainerInfo.getNetworkSettings.getPorts.getBindings.asScala
      .find { case (_, bindings) => bindings.head.getHostPortSpec == proxy.getProxyPort.toString }
      .map(_._1.getPort)
      .getOrElse(throw new IllegalStateException(s"There is no inner port for proxied one: ${proxy.getProxyPort}"))
  }
}
