package com.wavesplatform.dex.it.docker.base

import java.io.{FileNotFoundException, FileOutputStream}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import cats.instances.map.catsKernelStdMonoidForMap
import cats.instances.string.catsKernelStdMonoidForString
import cats.syntax.semigroup.catsSyntaxSemigroup
import com.dimafeng.testcontainers.GenericContainer
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.CreateNetworkCmd
import com.github.dockerjava.api.model.Network.Ipam
import com.github.dockerjava.api.model.{ContainerNetwork, ExposedPort, Ports}
import com.github.dockerjava.core.command.ExecStartResultCallback
import com.google.common.primitives.Ints.toByteArray
import com.spotify.docker.client.exceptions.ContainerNotFoundException
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.dex.it.docker.base.info.{BaseContainerInfo, DEXContainerInfo, WavesNodeContainerInfo}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import mouse.any._
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.io.IOUtils
import org.testcontainers.DockerClientFactory
import org.testcontainers.containers.Network
import org.testcontainers.containers.Network.NetworkImpl
import org.testcontainers.containers.wait.strategy.AbstractWaitStrategy
import org.testcontainers.utility.MountableFile

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.{Random, Try}

abstract class BaseContainer(underlying: GenericContainer) extends GenericContainer(underlying) with ScorexLogging {

  val name: String

  def prefix: String = BaseContainer.getPrefix(underlying)

  def restApiAddress: InetSocketAddress

  def getBasePath: String

  def getExternalAddress(internalPort: Int): InetSocketAddress = {

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
        .getOrElse(throw new IllegalStateException(s"There is no mapping '$internalPort/tcp' for '$name'"))

    new InetSocketAddress(underlying.containerIpAddress, externalPort)
  }

  def getInternalAddress(internalPort: Int): InetSocketAddress = {
    new InetSocketAddress(underlying.container.getContainerInfo.getNetworkSettings.getNetworks.values.asScala.head.getIpAddress, internalPort)
  }

  def replaceSuiteConfig(newSuiteConfig: Config): Unit

  protected def replaceInternalConfig(containerInfo: BaseContainerInfo, underlying: GenericContainer)(configName: String, config: Config): Unit = {

    val configContent =
      config
        .resolve()
        .root()
        .render(
          ConfigRenderOptions
            .concise()
            .setOriginComments(false)
            .setComments(false)
            .setFormatted(true)
            .setJson(false)
        )

    val containerPath = Paths.get(containerInfo.baseContainerPath, configName).toString
    val file          = BaseContainer.getMountableFileFromContent(configContent)

    underlying.configure(c => c.copyFileToContainer(file, containerPath))
  }

  private def printContainerState(): Unit = {

    val containerState = dockerClient.inspectContainerCmd(underlying.containerId).exec().getState

    log.debug(s"""$prefix Information:
                 |Exit code:  ${containerState.getExitCode}
                 |Error:      ${containerState.getError}
                 |Status:     ${containerState.getStatus}
                 |OOM killed: ${containerState.getOOMKilled}""".stripMargin)
  }

  private def copyFileFromContainer(containerPath: Path, localPath: Path): Unit =
    try {

      val is = dockerClient.copyArchiveFromContainerCmd(underlying.containerId, containerPath.toString).exec()
      val s  = new TarArchiveInputStream(is)

      Iterator.continually(s.getNextEntry).takeWhile(_ != null).take(1).foreach { _ =>
        val out = new FileOutputStream(localPath.toString)
        try IOUtils.copy(s, out)
        finally out.close()
      }

    } catch {
      case e: ContainerNotFoundException => log.warn(s"File '$containerPath' not found: ${e.getMessage}") // TODO REMOVE SPOTIFY EXCEPTION
    }

  private def saveContainerLogs(localLogsDir: Path): Unit = {

    val localDestination = localLogsDir.resolve(s"container-$name.log")

    log.info(s"$prefix Writing log to '${localDestination.toAbsolutePath}'")

    val openOption = if (Files exists localDestination) StandardOpenOption.APPEND else StandardOpenOption.CREATE

    Files.write(localDestination, container.getLogs.replace("\n\n", "\n") getBytes StandardCharsets.UTF_8, openOption)

    this match {
      case _: DEXContainer =>
        val containerSystemLogPath = Paths.get(getBasePath, "system.log")
        val localSystemLogPath     = localLogsDir.resolve(s"container-$name.system.log")

        log.info(s"$prefix Loading system log from '$containerSystemLogPath' to '$localSystemLogPath'")
        copyFileFromContainer(containerSystemLogPath, localSystemLogPath)
      case _ =>
    }
  }

  def printDebugMessage(text: String): Unit = {
    try {
      if (dockerClient.inspectContainerCmd(underlying.containerId).exec().getState.getRunning) {

        val escaped   = text.replace('\'', '\"')
        val execCmd   = dockerClient.execCreateCmd(underlying.containerId).withCmd("/bin/sh", "-c", s"/bin/echo '$escaped' >> /proc/1/fd/1")
        val execCmdId = execCmd.exec().getId

        try dockerClient.execStartCmd(execCmdId).exec(new ExecStartResultCallback)
        catch {
          case NonFatal(_) => /* ignore */
        } finally execCmd.close()
      }
    } catch {
      case _: ContainerNotFoundException => // TODO REMOVE SPOTIFY EXCEPTION
    }
  }

  def stopAndSaveLogs(implicit localLogsDir: Coeval[Path]): Unit = {
    printContainerState()
    log.debug(s"$prefix Stopping...")
    sendStopCmd()
    saveContainerLogs(localLogsDir.value)
    container.stop()
    dockerClient.close()
  }

  def sendStopCmd(): Unit  = dockerClient.stopContainerCmd(underlying.containerId).exec()
  def sendStartCmd(): Unit = dockerClient.startContainerCmd(underlying.containerId).exec()

  def disconnectFromNetwork(): Unit = {
    dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(underlying.containerId)
      .withNetworkId(BaseContainer.network.getId)
      .exec()
  }

  def connectToNetwork(): Unit = {
    dockerClient
      .connectToNetworkCmd()
      .withContainerId(underlying.containerId)
      .withNetworkId(BaseContainer.network.getId)
      .withContainerNetwork(new ContainerNetwork().withAliases(WavesNodeContainerInfo.netAlias))
      .exec()
  }
}

object BaseContainer extends ScorexLogging {

  val dockerClient: DockerClient = DockerClientFactory.instance.client

  private val ignoreWaitStrategy: AbstractWaitStrategy = () => ()

  private val networkSeed         = Random.nextInt(0x100000) << 4 | 0x0A000000 // a random network in 10.x.x.x range
  private val networkPrefix       = s"${InetAddress.getByAddress(toByteArray(networkSeed)).getHostAddress}/28" // 10.x.x.x/28 network will accommodate up to 13 nodes
  private val networkName: String = s"waves-${Random.nextInt(Int.MaxValue)}"

  val network: NetworkImpl = {
    Network
      .builder()
      .createNetworkCmdModifier { cmd: CreateNetworkCmd =>
        cmd.withIpam(new Ipam().withConfig(new Ipam.Config().withSubnet(networkPrefix).withIpRange(networkPrefix).withGateway(getIp(0xE))))
        cmd.withName(networkName)
      }
      .build()
  }

  def getIp(containerNumber: Int): String = InetAddress.getByAddress(toByteArray(containerNumber & 0xF | networkSeed)).getHostAddress

  def createDEXContainer(name: String, runConfig: Config, suiteInitialConfig: Config): DEXContainer = {
    DEXContainer(name, createBaseContainer(DEXContainerInfo)(name, runConfig, suiteInitialConfig))
  }

  def createWavesNodeContainer(name: String, runConfig: Config, suiteInitialConfig: Config): WavesNodeContainer = {
    WavesNodeContainer(name, createBaseContainer(WavesNodeContainerInfo)(name, runConfig, suiteInitialConfig))
  }

  private def createBaseContainer(containerInfo: BaseContainerInfo)(name: String, runConfig: Config, suiteInitialConfig: Config): GenericContainer = {

    import containerInfo._

    val containerIp = getIp(getNumber(name))
    val containerEnv = containerInfo match {
      case WavesNodeContainerInfo => getEnv(name) |+| Map("WAVES_OPTS" -> s" -Dwaves.network.declared-address=$containerIp:6883")
      case _                      => getEnv(name)
    }

    GenericContainer(
      dockerImage = image,
      exposedPorts = exposedPorts,
      env = containerEnv,
      waitStrategy = ignoreWaitStrategy
    ).configure { c =>
      Seq(
        (baseConfFileName, getRawContentFromResource(s"$baseLocalConfDir/$baseConfFileName"), false),
        (s"$name.conf", getRawContentFromResource(s"$baseLocalConfDir/$name.conf"), false),
        ("run.conf", runConfig.resolve().root().render(), true),
        ("suite.conf", suiteInitialConfig.resolve().root().render(), true),
        ("/doc/logback-container.xml", getRawContentFromResource("logback-container.xml"), true)
      ).foreach {
        case (fileName, content, logContent) =>
          val containerPath = Paths.get(baseContainerPath, fileName).toString

          if (logContent) log.trace(s"${getPrefix(name, c.getContainerId)} Write to '$containerPath':\n$content")

          c.withCopyFileToContainer(getMountableFileFromContent(content), containerPath)

          c.withNetwork(network)
          c.withNetworkAliases(containerInfo.netAlias)

          c.withCreateContainerCmdModifier { cmd =>
            cmd withName s"$networkName-$name"
            cmd withIpv4Address containerIp
          }
      }
    }
  }

  private def getPrefix(containerName: String, containerId: String): String = s"[name='$containerName', id=$containerId]"
  private def getPrefix(container: GenericContainer): String                = getPrefix(container.container.getContainerInfo.getName, container.containerId)

  private def getMountableFileFromContent(content: String): MountableFile = MountableFile.forHostPath {
    Files.createTempFile("suite", "") unsafeTap { Files.write(_, content getBytes StandardCharsets.UTF_8) }
  }

  private def getRawContentFromResource(fileName: String): String = {
    Try(Source fromResource fileName).getOrElse { throw new FileNotFoundException(s"Resource '$fileName'") }.mkString
  }

  private def getNumber(name: String): Int = {

    val raw =
      name
        .split('-')
        .lastOption
        .flatMap(x => Try(x.toInt).toOption)
        .getOrElse(throw new IllegalArgumentException(s"Can't parse the container's number: '$name'. It should have a form: <name>-<number>"))

    if (raw >= 5) throw new IllegalArgumentException("All slots are filled")
    else if (name.startsWith("dex-")) raw
    else if (name.startsWith("waves-")) raw + 5
    else throw new IllegalArgumentException(s"Can't parse number from '$name'. Know 'dex-' and 'waves-' only")
  }
}
