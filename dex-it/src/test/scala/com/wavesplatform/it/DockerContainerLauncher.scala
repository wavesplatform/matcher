package com.wavesplatform.it

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import cats.implicits._
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages.{ContainerConfig, EndpointConfig, HostConfig, PortBinding}
import com.wavesplatform.it.DockerContainerLauncher.{ContainerIsNotStartedYetError, DockerError}
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}

import scala.collection.JavaConverters._
import scala.util.Try

class DockerContainerLauncher(imageName: String,
                              containerName: String,
                              env: List[String],
                              containerIp: String,
                              containerPort: String,
                              networkName: String,
                              hostPort: Option[String] = None,
                              imageTag: String = "latest") {

  val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  private val hostConfig = {
    val hostConfigBuilder = HostConfig.builder()
    hostPort
      .fold(hostConfigBuilder.publishAllPorts(true)) { port =>
        hostConfigBuilder.portBindings(Map(containerPort -> List(PortBinding.of("0.0.0.0", port)).asJava).asJava)
      }
      .build()
  }

  private def endpointConfig: EndpointConfig = {
    EndpointConfig
      .builder()
      .ipAddress(containerIp)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(containerIp).build())
      .build()
  }

  private val containerConfig = {
    dockerClient.pull(s"$imageName:$imageTag")
    ContainerConfig
      .builder()
      .hostConfig(hostConfig)
      .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(networkName -> endpointConfig).asJava))
      .exposedPorts(containerPort)
      .image(imageName)
      .env(env: _*)
      .build()
  }

  private val creation    = dockerClient.createContainer(containerConfig, containerName)
  val containerId: String = creation.id()

  def getHostPort: Either[DockerError, String] = {
    hostPort.fold {
      Either
        .fromTry(Try { dockerClient.inspectContainer(containerId).networkSettings().ports().asScala(s"$containerPort/tcp").asScala.head.hostPort() })
        .leftMap[DockerError](ex => ContainerIsNotStartedYetError(s"Container with name $containerName is not started yet, ex: ${ex.getMessage}"))
    } { _.asRight[DockerError] }
  }

  def startContainer(): Unit = dockerClient.startContainer(containerId)

  def stopAndRemoveContainer(): Unit = {
    dockerClient.stopContainer(containerId, 0)
    dockerClient.removeContainer(containerId, RemoveContainerParam.removeVolumes())
  }

  def writeFile(to: Path, content: String): Unit = {

    val os    = new ByteArrayOutputStream()
    val s     = new TarArchiveOutputStream(os)
    val bytes = content.getBytes(StandardCharsets.UTF_8)
    val entry = new TarArchiveEntry(s"${to.getFileName}")

    entry.setSize(bytes.size)
    s.putArchiveEntry(entry)
    s.write(bytes)
    s.closeArchiveEntry()

    val is = new ByteArrayInputStream(os.toByteArray)
    s.close()

    try dockerClient.copyToContainer(is, containerId, s"${to.getParent.toString}")
    finally is.close()
  }
}

object DockerContainerLauncher {
  trait DockerError                                   extends Product with Serializable
  case class ContainerIsNotStartedYetError(m: String) extends DockerError
}
