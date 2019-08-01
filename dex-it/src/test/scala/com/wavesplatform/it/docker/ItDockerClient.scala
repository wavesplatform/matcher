package com.wavesplatform.it.docker

import java.net.InetSocketAddress
import java.util.Properties

import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.{ContainerConfig, EndpointConfig, HostConfig, Network}
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.dex.api.grpc.WavesBlockchainApiGrpc
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.it.config.DexTestConfig.WavesNodeConfig
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.Dsl.{config => clientConfig, _}

import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import ItDockerClient._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.wavesplatform.it.Docker.{jsonMapper, propsMapper}

class ItDockerClient(client: DockerClient, network: Network) extends ScorexLogging {
  def createWavesNode(config: Config, ip: String): NodeContainer =
    try {
      val actualConfig = config.resolve()

      val networkPort = actualConfig.getString("waves.network.port")
      val hostConfig  = HostConfig.builder().publishAllPorts(true).build()

      val nodeName = actualConfig.getString("waves.network.node-name")

      val javaOptions = Option(System.getenv("CONTAINER_JAVA_OPTS")).getOrElse("")
      val configOverrides: String = {
        val ntpServer    = Option(System.getenv("NTP_SERVER")).fold("")(x => s"-Dwaves.ntp-server=$x ")
        val maxCacheSize = Option(System.getenv("MAX_CACHE_SIZE")).fold("")(x => s"-Dwaves.max-cache-size=$x ")

        var config = s"$javaOptions ${renderProperties(asProperties(actualConfig))} " +
          s"-Dlogback.stdout.level=TRACE -Dlogback.file.level=OFF -Dwaves.network.declared-address=$ip:$networkPort $ntpServer $maxCacheSize"

//        if (enableProfiling) {
//          // https://www.yourkit.com/docs/java/help/startup_options.jsp
//          config += s"-agentpath:/usr/local/YourKit-JavaProfiler-2019.1/bin/linux-x86-64/libyjpagent.so=port=$ProfilerPort,listen=all," +
//            s"sampling,monitors,sessionname=WavesNode,dir=$ContainerRoot/profiler,logdir=$ContainerRoot,onexit=snapshot "
//        }

//        val withAspectJ = Option(System.getenv("WITH_ASPECTJ")).fold(false)(_.toBoolean)
//        if (withAspectJ) config += s"-javaagent:$ContainerRoot/aspectjweaver.jar "
        config
      }

      val containerConfig = ContainerConfig
        .builder()
        .image(wavesNodeImage)
        .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(network.name() -> endpointConfigFor(ip)).asJava))
        .hostConfig(hostConfig)
        .env(s"WAVES_OPTS=$configOverrides")
        .build()

      val containerId = {
        val containerName = s"${network.name()}-$nodeName"
//        dumpContainers(
//          client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
//          "Containers with same name"
//        )

        log.debug(s"Creating container $containerName at $ip with options: $javaOptions")
        val r = client.createContainer(containerConfig, containerName)
        Option(r.warnings().asScala).toSeq.flatten.foreach(log.warn(_))
        r.id()
      }

      client.startContainer(containerId)

      val node = new NodeContainer(containerId, actualConfig)
//      nodes.add(node)
//      log.debug(s"Started $containerId -> ${node.name}: ${node.nodeInfo}")
      node
    } catch {
      case NonFatal(e) =>
        log.error("Can't start a container", e)
//        dumpContainers(client.listContainers())
        throw e
    }

  private def endpointConfigFor(ip: String): EndpointConfig =
    EndpointConfig
      .builder()
      .ipAddress(ip)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
      .build()

  def createDexNode(config: Config): DexContainer = ???

  def start(container: DockerContainer): Unit = client.startContainer(container.id)

  def stop(container: DockerContainer): Unit = client.stopContainer(container.id, 10)

  def disconnectFromNetwork(container: DockerContainer): Unit = client.disconnectFromNetwork(container.id, network.id())

  def connectToNetwork(container: DockerContainer): Unit = client.connectToNetwork(container.id, network.id())

  def getInetSocketAddress(container: DockerContainer, internalPort: Int): InetSocketAddress = {
    val binding = client.inspectContainer(container.id).networkSettings().ports().get(s"$internalPort/tcp").get(0)
    new InetSocketAddress(binding.hostIp(), binding.hostPort().toInt)
  }
}

object ItDockerClient {
  val wavesNodeImage = "com.wavesplatform/node-it:latest"
  val dexNodeImage   = "com.wavesplatform/dex-it:latest"

  private val jsonMapper  = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  def renderProperties(p: Properties) =
    p.asScala
      .map {
        case (k, v) if v.contains(" ") => k -> s""""$v""""
        case x                         => x
      }
      .map { case (k, v) => s"-D$k=$v" }
      .mkString(" ")
}

abstract class DockerContainer {
  def id: String
}

class NodeContainer(override val id: String, config: Config) extends DockerContainer {
  val settings: WavesSettings = WavesSettings.fromRootConfig(config.resolve())
}

class DexContainer(override val id: String, config: Config) extends DockerContainer {
  val settings: MatcherSettings = MatcherSettings.valueReader.read(config.resolve(), "waves.dex")
}
