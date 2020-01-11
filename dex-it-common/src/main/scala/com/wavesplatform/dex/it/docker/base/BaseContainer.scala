package com.wavesplatform.dex.it.docker.base

import java.io.{FileNotFoundException, FileOutputStream}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets
import java.nio.file._

import cats.Id
import cats.instances.map.catsKernelStdMonoidForMap
import cats.instances.string.catsKernelStdMonoidForString
import cats.syntax.semigroup.catsSyntaxSemigroup
import com.dimafeng.testcontainers.GenericContainer
import com.github.dockerjava.api.command.CreateNetworkCmd
import com.github.dockerjava.api.exception.NotFoundException
import com.github.dockerjava.api.model.Network.Ipam
import com.github.dockerjava.api.model.{ContainerNetwork, ExposedPort, Ports}
import com.github.dockerjava.core.command.ExecStartResultCallback
import com.google.common.primitives.Ints.toByteArray
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.docker.base.BaseContainer.{getIp, getNumber}
import com.wavesplatform.dex.it.docker.base.info.{BaseContainerInfo, DexContainerInfo, WavesNodeContainerInfo}
import com.wavesplatform.utils.ScorexLogging
import mouse.any._
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.testcontainers.containers.Network
import org.testcontainers.containers.Network.NetworkImpl
import org.testcontainers.containers.wait.strategy.AbstractWaitStrategy
import org.testcontainers.shaded.org.apache.commons.io.IOUtils
import org.testcontainers.utility.MountableFile

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.{Random, Try}

abstract class BaseContainer(underlying: GenericContainer, baseContainerInfo: BaseContainerInfo)
    extends GenericContainer(underlying)
    with ScorexLogging {

  protected val name: String
  protected val internalIp: String = getIp(getNumber(name))

  protected def prefix: String = BaseContainer.getPrefix(underlying)

  protected val cachedRestApiAddress: CachedData[InetSocketAddress]

  def api: HasWaitReady[Id]
  def asyncApi: HasWaitReady[Future]

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
        .getOrElse(throw new IllegalStateException(s"There is no mapping '$internalPort/tcp' for '$name'"))

    new InetSocketAddress(underlying.containerIpAddress, externalPort)
  }

  protected def getInternalAddress(internalPort: Int): InetSocketAddress = new InetSocketAddress(internalIp, internalPort)

  private def printState(): Unit = {

    val containerState = dockerClient.inspectContainerCmd(underlying.containerId).exec().getState

    log.debug(s"""$prefix Information:
                 |Exit code:  ${containerState.getExitCode}
                 |Error:      ${containerState.getError}
                 |Status:     ${containerState.getStatus}
                 |OOM killed: ${containerState.getOOMKilled}""".stripMargin)
  }

  private def replaceSuiteConfig(newSuiteConfig: Config): Unit = {

    val configContent =
      newSuiteConfig
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

    val containerPath = Paths.get(baseContainerInfo.baseContainerPath, "suite.conf").toString
    val file          = BaseContainer.getMountableFileFromContent(configContent)

    underlying.configure(c => c.copyFileToContainer(file, containerPath))
  }

  protected def copyFileToLocalPath(containerPath: Path, localPath: Path): Unit =
    try {

      val is = dockerClient.copyArchiveFromContainerCmd(underlying.containerId, containerPath.toString).exec()
      val s  = new TarArchiveInputStream(is)

      Iterator.continually(s.getNextEntry).takeWhile(_ != null).take(1).foreach { _ =>
        val out = new FileOutputStream(localPath.toString)
        try IOUtils.copy(s, out)
        finally out.close()
      }

    } catch {
      case e: NotFoundException => log.warn(s"File '$containerPath' not found: ${e.getMessage}")
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
      case _: NotFoundException =>
    }
  }

  def stopWithoutRemove(): Unit = {
    printState()
    log.debug(s"$prefix Stopping...")

    dockerClient.stopContainerCmd(underlying.containerId).withTimeout(20).exec()
    Iterator
      .continually {
        Thread.sleep(1000)
        dockerClient.inspectContainerCmd(underlying.containerId).exec().getState
      }
      .zipWithIndex
      .find { case (state, attempt) => !state.getRunning || attempt == 20 }
      .fold(log.warn(s"Can't stop ${underlying.containerId}"))(_ => ())
  }

  private def sendStartCmd(): Unit = dockerClient.startContainerCmd(underlying.containerId).exec()

  def disconnectFromNetwork(): Unit = {
    dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(underlying.containerId)
      .withNetworkId(BaseContainer.network.getId)
      .exec()
  }

  def invalidateCaches(): Unit = cachedRestApiAddress.invalidate()

  def connectToNetwork(): Unit = {
    invalidateCaches()

    dockerClient
      .connectToNetworkCmd()
      .withContainerId(underlying.containerId)
      .withNetworkId(BaseContainer.network.getId)
      .withContainerNetwork(
        new ContainerNetwork()
          .withIpamConfig(new ContainerNetwork.Ipam().withIpv4Address(internalIp))
          .withAliases(WavesNodeContainerInfo.netAlias))
      .exec()

    api.waitReady
  }

  override def start(): Unit = {
    Option(underlying.containerId).fold { super.start() }(_ => sendStartCmd())
    invalidateCaches()
    api.waitReady
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

object BaseContainer extends ScorexLogging {

  val apiKey                      = "integration-test-rest-api"
  val isProfilingEnabled: Boolean = Option(System.getenv("WAVES_DEX_PROFILING")).getOrElse("false").toBoolean

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

  def create(containerInfo: BaseContainerInfo)(name: String, runConfig: Config, suiteInitialConfig: Config): GenericContainer = {

    import containerInfo._

    val ip: String                               = getIp(getNumber(name))
    val ignoreWaitStrategy: AbstractWaitStrategy = () => ()

    val env = getEnv(name) |+| {
      val commonOpts = List(
        s"-Dlogback.brief.fullPath=${containerInfo.containerLogsPath}/container-$name.log",
        s"-Dlogback.detailed.fullPath=${containerInfo.containerLogsPath}/container-$name.detailed.log"
      )

      containerInfo match {
        case WavesNodeContainerInfo =>
          val opts = s"-Dwaves.network.declared-address=$ip:6883" :: commonOpts
          Map("WAVES_OPTS" -> opts.mkString(" ", " ", " "))
        case DexContainerInfo =>
          val opts = {
            // https://www.yourkit.com/docs/java/help/startup_options.jsp
            if (isProfilingEnabled)
              List(
                s"-J-agentpath:/usr/local/YourKit-JavaProfiler-2019.8/bin/linux-x86-64/libyjpagent.so=port=10001,listen=all" +
                  s",sampling,monitors,sessionname=prof-$name,snapshot_name_format={sessionname}," +
                  s"dir=${containerInfo.containerLogsPath},logdir=${containerInfo.containerLogsPath},onexit=snapshot"
              )
            else List.empty[String]
          } ::: commonOpts
          Map("WAVES_DEX_OPTS" -> opts.mkString(" ", " ", " "))
      }
    }

    GenericContainer(
      dockerImage = image,
      exposedPorts = exposedPorts,
      env = env,
      waitStrategy = ignoreWaitStrategy
    ).configure { c =>
      c.withNetwork(network)
      c.withNetworkAliases(containerInfo.netAlias)
      c.withCreateContainerCmdModifier { cmd =>
        cmd withName s"$networkName-$name"
        cmd withIpv4Address ip
      }

      val filesToCopy = containerInfo.specificFiles ++ Seq(
        (baseConfFileName, getRawContentFromResource(s"$baseLocalConfDir/$baseConfFileName"), false),
        (s"$name.conf", getRawContentFromResource(s"$baseLocalConfDir/$name.conf"), false),
        ("run.conf", runConfig.resolve().root().render(), true),
        ("suite.conf", suiteInitialConfig.resolve().root().render(), true)
      )

      filesToCopy.foreach {
        case (fileName, content, logContent) =>
          val containerPath = Paths.get(baseContainerPath, fileName).toString
          val msg = s"${getPrefix(name, c.getContainerId)} Write to '$containerPath'"
          log.trace(s"$msg${if (logContent) s":\n$content" else ""}")
          c.withCopyFileToContainer(getMountableFileFromContent(content), containerPath)
      }
    }
  }

  private def getPrefix(containerName: String, containerId: String): String = s"[name='$containerName', id=$containerId]"
  private def getPrefix(container: GenericContainer): String                = getPrefix(container.container.getContainerInfo.getName, container.containerId)

  private def getMountableFileFromContent(content: String): MountableFile = MountableFile.forHostPath {
    Files.createTempFile("tmp", "") unsafeTap { Files.write(_, content getBytes StandardCharsets.UTF_8) }
  }

  def getRawContentFromResource(fileName: String): String = {
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
