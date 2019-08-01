package com.wavesplatform.it.docker

import java.net.InetSocketAddress

import com.spotify.docker.client.messages.HostConfig
import com.typesafe.config.Config
import com.wavesplatform.dex.api.grpc.WavesBlockchainApiGrpc
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.settings.WavesSettings
import io.grpc.ManagedChannel
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.Dsl.{config => clientConfig, _}

object ItDockerClient {
  def createNode(config: Config): NodeContainer = ???
//    try {
//      val actualConfig = config.resolve()
//
//      val networkPort = actualConfig.getString("waves.network.port")
//      val hostConfig  = HostConfig.builder().publishAllPorts(true).build()
//
//      val nodeName   = actualConfig.getString("waves.network.node-name")
//      val nodeNumber = nodeName.replace("node", "").toInt
//      val ip         = ipForNode(nodeNumber)
//
//      val javaOptions = Option(System.getenv("CONTAINER_JAVA_OPTS")).getOrElse("")
//      val configOverrides: String = {
//        val ntpServer    = Option(System.getenv("NTP_SERVER")).fold("")(x => s"-Dwaves.ntp-server=$x ")
//        val maxCacheSize = Option(System.getenv("MAX_CACHE_SIZE")).fold("")(x => s"-Dwaves.max-cache-size=$x ")
//
//        var config = s"$javaOptions ${renderProperties(asProperties(overrides))} " +
//          s"-Dlogback.stdout.level=TRACE -Dlogback.file.level=OFF -Dwaves.network.declared-address=$ip:$networkPort $ntpServer $maxCacheSize"
//
//        if (enableProfiling) {
//          // https://www.yourkit.com/docs/java/help/startup_options.jsp
//          config += s"-agentpath:/usr/local/YourKit-JavaProfiler-2019.1/bin/linux-x86-64/libyjpagent.so=port=$ProfilerPort,listen=all," +
//            s"sampling,monitors,sessionname=WavesNode,dir=$ContainerRoot/profiler,logdir=$ContainerRoot,onexit=snapshot "
//        }
//
//        val withAspectJ = Option(System.getenv("WITH_ASPECTJ")).fold(false)(_.toBoolean)
//        if (withAspectJ) config += s"-javaagent:$ContainerRoot/aspectjweaver.jar "
//        config
//      }
//
//      val containerConfig = ContainerConfig
//        .builder()
//        .image(imageName)
//        .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(wavesNetwork.name() -> endpointConfigFor(nodeName)).asJava))
//        .hostConfig(hostConfig)
//        .env(s"WAVES_OPTS=$configOverrides")
//        .build()
//
//      val containerId = {
//        val containerName = s"${wavesNetwork.name()}-$nodeName"
//        dumpContainers(
//          client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
//          "Containers with same name"
//        )
//
//        log.debug(s"Creating container $containerName at $ip with options: $javaOptions")
//        val r = client.createContainer(containerConfig, containerName)
//        Option(r.warnings().asScala).toSeq.flatten.foreach(log.warn(_))
//        r.id()
//      }
//
//      client.startContainer(containerId)
//
//      val node = new NodeContainer(actualConfig, containerId, getNodeInfo(containerId, WavesSettings.fromRootConfig(actualConfig)))
//      nodes.add(node)
//      log.debug(s"Started $containerId -> ${node.name}: ${node.nodeInfo}")
//      node
//    } catch {
//      case NonFatal(e) =>
//        log.error("Can't start a container", e)
//        dumpContainers(client.listContainers())
//        throw e
//    }

  def createDex(config: Config): DexContainer = ???

  def start(container: DockerContainer): Unit = ???

  def stop(container: DockerContainer): Unit = ???

  def disconnectFromNetwork(container: DockerContainer): Unit = ???

  def connectToNetwork(container: DockerContainer): Unit = ???

  def get
}

abstract class DockerContainer extends AutoCloseable {
  val grpcChannel: ManagedChannel = ???
  override def close(): Unit = grpcChannel.shutdownNow()
}

class NodeContainer() extends DockerContainer {
  val settings: WavesSettings = ???
}

class DexContainer() extends DockerContainer {
  val settings: MatcherSettings = ???
}
