package com.wavesplatform.dex.it.docker

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file._

import com.dimafeng.testcontainers.GenericContainer
import com.github.dockerjava.api.async.ResultCallback
import com.github.dockerjava.api.command.InspectContainerResponse
import com.github.dockerjava.api.exception.{NotFoundException, NotModifiedException}
import com.github.dockerjava.api.model.{ContainerNetwork, ExposedPort, Frame, Ports}
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import org.testcontainers.images.builder.Transferable

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

abstract class BaseContainer(protected val baseContainerPath: String, private val underlying: GenericContainer)
    extends GenericContainer(underlying)
    with ScorexLogging {

  protected def internalIp: String

  protected def prefix: String = s"[name=${underlying.containerInfo.getName}, id=${underlying.containerInfo.getId}]"

  protected val cachedRestApiAddress: CachedData[InetSocketAddress]

  def waitReady(): Unit

  protected def getExternalAddress(internalPort: Int): InetSocketAddress = {

    val maybeBindings: Option[Array[Ports.Binding]] =
      dockerClient
        .inspectContainerCmd(underlying.containerId)
        .exec()
        .getNetworkSettings
        .getPorts
        .getBindings
        .asScala
        .get(new ExposedPort(internalPort))

    val externalPort =
      maybeBindings
        .flatMap(_.headOption)
        .map(_.getHostPortSpec.toInt)
        .getOrElse(throw new IllegalStateException(s"There is no mapping '$internalPort/tcp' for '${underlying.containerName}'"))

    new InetSocketAddress(underlying.containerIpAddress, externalPort)
  }

  protected def getInternalAddress(internalPort: Int): InetSocketAddress = new InetSocketAddress(internalIp, internalPort)

  private def printState(): Unit = {
    val containerState = getState()
    log.debug(s"""$prefix Information:
                 |Exit code:  ${containerState.getExitCodeLong}
                 |Error:      ${containerState.getError}
                 |Status:     ${containerState.getStatus}
                 |OOM killed: ${containerState.getOOMKilled}""".stripMargin)
  }

  def replaceSuiteConfig(newSuiteConfig: Config): Unit = underlying.configure { c =>
    val containerPath = Paths.get(baseContainerPath, "suite.conf").toString
    val content = newSuiteConfig.rendered
    log.trace(s"$prefix Write to '$containerPath':\n$content")
    c.copyFileToContainer(Transferable.of(content.getBytes(StandardCharsets.UTF_8)), containerPath)
  }

  def getState(): InspectContainerResponse#ContainerState = dockerClient.inspectContainerCmd(underlying.containerId).exec().getState

  def printDebugMessage(text: String): Unit =
    try if (Option(underlying.containerId).exists(dockerClient.inspectContainerCmd(_).exec().getState.getRunning)) {

      val escaped = text.replace('\'', '\"')

      val execCmd =
        dockerClient
          .execCreateCmd(underlying.containerId)
          .withCmd(
            "/bin/sh",
            "-c",
            s"""/bin/echo '$escaped' >> $$BRIEF_LOG_PATH; /bin/echo '$escaped' >> $$DETAILED_LOG_PATH"""
          )

      val execCmdId = execCmd.exec().getId

      try dockerClient.execStartCmd(execCmdId).exec(new ResultCallback.Adapter[Frame])
      catch {
        case NonFatal(_) => /* ignore */
      } finally execCmd.close()
    } catch {
      case _: NotFoundException =>
    }

  def stopWithoutRemove(): Unit = {
    printState()
    log.debug(s"$prefix Stopping...")

    try {
      dockerClient.stopContainerCmd(underlying.containerId).withTimeout(20).exec()
      Iterator
        .continually {
          Thread.sleep(1000)
          dockerClient.inspectContainerCmd(underlying.containerId).exec().getState
        }
        .zipWithIndex
        .find { case (state, attempt) => !state.getRunning || attempt == 20 }
        .fold(log.warn(s"Can't stop ${underlying.containerId}"))(_ => ())
    } catch {
      case e: NotModifiedException => log.warn(s"$prefix Can't stop", e)
      case e: Throwable => throw e
    }
  }

  private def sendStartCmd(): Unit = dockerClient.startContainerCmd(underlying.containerId).exec()

  def disconnectFromNetwork(): Unit =
    dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(underlying.containerId)
      .withNetworkId(underlying.network.getId)
      .exec()

  def invalidateCaches(): Unit = cachedRestApiAddress.invalidate()

  def reconnectToNetwork(delay: Long = 0, duration: Long = 100): Unit = {
    Thread.sleep(delay)
    disconnectFromNetwork()
    Thread.sleep(duration)
    connectToNetwork()
  }

  def connectToNetwork(): Unit = {
    dockerClient
      .connectToNetworkCmd()
      .withContainerId(underlying.containerId)
      .withNetworkId(underlying.network.getId)
      .withContainerNetwork(
        new ContainerNetwork()
          .withIpamConfig(new ContainerNetwork.Ipam().withIpv4Address(internalIp))
          .withAliases(underlying.networkAliases.asJava)
      )
      .exec()

    Iterator
      .continually {
        Thread.sleep(1000)
        dockerClient.inspectContainerCmd(underlying.containerId).exec().getNetworkSettings
      }
      .zipWithIndex
      .find { case (ns, attempt) =>
        ns.getNetworks.asScala.exists(_._2.getNetworkID == underlying.network.getId) || attempt == 20
      }
      .fold(log.warn(s"Can't start ${underlying.containerId}"))(_ => ())

    invalidateCaches()
    waitReady()
  }

  override def start(): Unit = {
    Option(underlying.containerId).fold(super.start())(_ => sendStartCmd())

    Iterator
      .continually {
        Thread.sleep(1000)
        dockerClient.inspectContainerCmd(underlying.containerId).exec().getState
      }
      .zipWithIndex
      .find { case (state, attempt) => state.getRunning || attempt == 20 }
      .fold(log.warn(s"Can't start ${underlying.containerId}"))(_ => ())

    invalidateCaches()
    waitReady()
  }

  def restart(): Unit = {
    stopWithoutRemove()
    start()
  }

  def restartWithNewSuiteConfig(newSuiteConfig: Config): Unit = {
    replaceSuiteConfig(newSuiteConfig)
    restart()
  }

}
