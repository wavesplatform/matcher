package com.wavesplatform.dex.it.tools

import cats.implicits._
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages.{ContainerConfig, EndpointConfig, HostConfig, PortBinding}
import com.wavesplatform.dex.it.tools.DockerContainerLauncher.{ContainerIsNotStartedYetError, DockerError, VolumePair}
import mouse.any._

import scala.collection.JavaConverters._
import scala.util.Try

class DockerContainerLauncher(imageName: String,
                              containerName: String,
                              containerIp: String,
                              containerPort: String,
                              imageTag: String = "",
                              env: String = "",
                              networkName: String = "",
                              hostPort: Option[String] = None,
                              volumePath: Option[VolumePair] = None,
                              capabilities: Seq[String] = Seq.empty) {

  val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  private val hostConfig: HostConfig =
    HostConfig.builder() |> { builder =>
      hostPort.fold(builder.publishAllPorts(true)) { port =>
        builder.portBindings(Map(containerPort -> List(PortBinding.of("0.0.0.0", port)).asJava).asJava)
      }
    } |> { builder =>
      volumePath.fold(builder) { case VolumePair(localPath, containerPath) => builder.appendBinds(s"$localPath:$containerPath") }
    } |> { builder =>
      Option(capabilities).filter(_.nonEmpty).fold(builder)(caps => builder.capAdd(caps: _*))
    } |> (_.build)

  private def endpointConfig: EndpointConfig = {
    EndpointConfig
      .builder()
      .ipAddress(containerIp)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(containerIp).build())
      .build()
  }

  private val containerConfig: ContainerConfig = {
    s"$imageName${if (imageTag.nonEmpty) s":$imageTag" else ""}" |> { image =>
      dockerClient.pull(image)
      ContainerConfig
        .builder()
        .hostConfig(hostConfig)
        .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(networkName -> endpointConfig).asJava))
        .exposedPorts(containerPort)
        .image(image) |> (builder => if (env.nonEmpty) builder.env(env) else builder) |> (_.build)
    }
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
}

object DockerContainerLauncher {

  trait DockerError                                   extends Product with Serializable
  case class ContainerIsNotStartedYetError(m: String) extends DockerError

  case class VolumePair(localPath: String, containerPath: String)
}
