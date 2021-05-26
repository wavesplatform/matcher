package com.wavesplatform.dex.it.docker

import com.dimafeng.testcontainers.GenericContainer
import org.testcontainers.containers.{GenericContainer => JavaGenericContainer}
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.InspectContainerResponse
import eu.rekawek.toxiproxy.Proxy
import eu.rekawek.toxiproxy.ToxiproxyClient
import eu.rekawek.toxiproxy.model.{ToxicDirection, ToxicList}
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy
import org.testcontainers.utility.DockerImageName

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class ConfigurableToxicProxyContainer(image: String, maxExposedPorts: Int = 0) extends GenericContainer(GenericContainer(dockerImage = image)) {

  import ConfigurableToxicProxyContainer._

  private val DEFAULT_IMAGE_NAME: DockerImageName = DockerImageName.parse("shopify/toxiproxy")
  private val TOXIPROXY_CONTROL_PORT: Int = 8474
  private val FIRST_PROXIED_PORT: Int = 8666
  private val LAST_PROXIED_PORT: Int = 8666 + maxExposedPorts

  private var client: Option[ToxiproxyClient] = None
  private val proxies = mutable.Map.empty[String, ContainerProxy]
  private val nextPort = new AtomicInteger(FIRST_PROXIED_PORT)

  private val parsedDockerImageName = DockerImageName.parse(image)

  parsedDockerImageName.assertCompatibleWith(DEFAULT_IMAGE_NAME)
  container.addExposedPorts(TOXIPROXY_CONTROL_PORT)
  container.setWaitStrategy(new HttpWaitStrategy().forPath("/version").forPort(TOXIPROXY_CONTROL_PORT))

  for (i <- FIRST_PROXIED_PORT to LAST_PROXIED_PORT)
    container.addExposedPort(i)

  override def start(): Unit = {
    super.start()
    setUpClient()
  }

  protected def setUpClient(): Unit =
    client = Some(new ToxiproxyClient(container.getHost, container.getMappedPort(TOXIPROXY_CONTROL_PORT)))

  def getControlPort: Int = container.getMappedPort(TOXIPROXY_CONTROL_PORT)

  def getContainerInfo: InspectContainerResponse = containerInfo

  def getDockerClient: DockerClient = container.getDockerClient
  def getContainerId: String = container.getContainerId

  def getProxy(targetContainer: JavaGenericContainer[_], port: Int): ContainerProxy =
    getProxy(targetContainer.getNetworkAliases.get(0), port)

  def getProxy(hostname: String, port: Int): ContainerProxy = {
    val upstream: String = hostname + ":" + port
    proxies.getOrElse(
      upstream,
      getNewProxy(upstream)
    )
  }

  private def getNewProxy(upstream: String): ContainerProxy = {
    val toxiPort: Int = nextPort.getAndIncrement
    if (toxiPort > LAST_PROXIED_PORT)
      throw new IllegalStateException("Maximum number of proxies exceeded")
    val proxy = client.map(_.createProxy(upstream, "0.0.0.0:" + toxiPort, upstream))
      .getOrElse(throw new RuntimeException("Cannot get proxy from toxiProxy because client isn't presented"))

    val mappedPort = container.getMappedPort(toxiPort)
    val newContainerProxy = ContainerProxy(proxy, container.getHost, mappedPort, toxiPort)
    proxies.put(upstream, newContainerProxy)
    newContainerProxy
  }

}

object ConfigurableToxicProxyContainer {

  object ContainerProxy {
    private val CUT_CONNECTION_DOWNSTREAM: String = "CUT_CONNECTION_DOWNSTREAM"
    private val CUT_CONNECTION_UPSTREAM: String = "CUT_CONNECTION_UPSTREAM"
  }

  case class ContainerProxy(toxi: Proxy, containerIpAddress: String, proxyPort: Int, originalProxyPort: Int) {

    private var isCurrentlyCut: Boolean = false

    def getName: String = toxi.getName

    def toxics: ToxicList = toxi.toxics

    def setConnectionCut(shouldCutConnection: Boolean): Unit =
      if (shouldCutConnection) {
        toxics.bandwidth(ContainerProxy.CUT_CONNECTION_DOWNSTREAM, ToxicDirection.DOWNSTREAM, 0)
        toxics.bandwidth(ContainerProxy.CUT_CONNECTION_UPSTREAM, ToxicDirection.UPSTREAM, 0)
        isCurrentlyCut = true
      } else if (isCurrentlyCut) {
        toxics.get(ContainerProxy.CUT_CONNECTION_DOWNSTREAM).remove()
        toxics.get(ContainerProxy.CUT_CONNECTION_UPSTREAM).remove()
        isCurrentlyCut = false
      }

  }

}
