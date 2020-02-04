package com.wavesplatform.dex.it.docker

import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}

import cats.Id
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.dex.it.api.node.NodeApi
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.resources.getRawContentFromResource
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.Network.NetworkImpl

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class WavesNodeContainer(override val internalIp: String, underlying: GenericContainer)(
    implicit
    tryHttpBackend: LoggingSttpBackend[Try, Nothing],
    futureHttpBackend: LoggingSttpBackend[Future, Nothing],
    ec: ExecutionContext)
    extends BaseContainer(WavesNodeContainer.baseContainerPath, underlying) {

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(WavesNodeContainer.restApiPort))

  private val cachedNetworkAddress = CachedData(getInternalAddress(WavesNodeContainer.networkPort))
  private val cachedGrpcApiAddress = CachedData(getExternalAddress(WavesNodeContainer.dexGrpcExtensionPort))

  def networkAddress: InetSocketAddress = cachedNetworkAddress.get()
  def grpcApiAddress: InetSocketAddress = cachedGrpcApiAddress.get()

  def grpcApiTarget: String = s"${grpcApiAddress.getHostName}:${grpcApiAddress.getPort}"

  override def api: NodeApi[Id]               = fp.sync { NodeApi[Try](apiKey, cachedRestApiAddress.get()) }
  override def asyncApi: HasWaitReady[Future] = NodeApi[Future](apiKey, cachedRestApiAddress.get())

  override def invalidateCaches(): Unit = {
    super.invalidateCaches()
    cachedNetworkAddress.invalidate()
    cachedGrpcApiAddress.invalidate()
  }
}

object WavesNodeContainer extends ScorexLogging {

  private val baseContainerPath: String = "/opt/waves"
  private val containerLogsPath: String = s"$baseContainerPath/logs"

  private val restApiPort: Int  = 6869 // application.conf waves.rest-api.port
  private val networkPort: Int  = 6863 // application.conf waves.network.port
  val dexGrpcExtensionPort: Int = 6887 // application.conf waves.dex.grpc.integration.port

  val netAlias: String = "waves.nodes"

  def apply(name: String,
            networkName: String,
            network: NetworkImpl,
            internalIp: String,
            runConfig: Config,
            suiteInitialConfig: Config,
            localLogsDir: Path)(implicit
                                tryHttpBackend: LoggingSttpBackend[Try, Nothing],
                                futureHttpBackend: LoggingSttpBackend[Future, Nothing],
                                ec: ExecutionContext): WavesNodeContainer = {

    val underlying = GenericContainer(
      dockerImage = "com.wavesplatform/waves-integration-it:latest",
      exposedPorts = List(restApiPort, networkPort, dexGrpcExtensionPort),
      env = getEnv(name, internalIp),
      waitStrategy = ignoreWaitStrategy
    ).configure { c =>
      c.withNetwork(network)
      c.withNetworkAliases(netAlias)
      c.withFileSystemBind(localLogsDir.toString, containerLogsPath, BindMode.READ_WRITE)
      c.withCreateContainerCmdModifier {
        _.withName(s"$networkName-$name") // network.getName returns random id
          .withIpv4Address(internalIp): Unit
      }

      // Copy files to container
      List(
        ("waves-base.conf", getRawContentFromResource(s"nodes/waves-base.conf"), false),
        (s"$name.conf", getRawContentFromResource(s"nodes/$name.conf"), false),
        ("run.conf", runConfig.rendered, true),
        ("suite.conf", suiteInitialConfig.rendered, true),
        ("logback-container.xml", getRawContentFromResource("nodes/logback-container.xml"), false),
        ("jul.properties", getRawContentFromResource("nodes/jul.properties"), false)
      ).foreach {
        case (fileName, content, logContent) =>
          val containerPath = Paths.get(baseContainerPath, fileName).toString
          log.trace(s"[name=$name] Write to '$containerPath'${if (logContent) s":\n$content" else ""}")
          c.withCopyFileToContainer(MountableFileOps.fromContent(content), containerPath)
      }
    }

    WavesNodeContainer(internalIp, underlying)
  }

  private def getEnv(containerName: String, ip: String): Map[String, String] = Map(
    "BRIEF_LOG_PATH"        -> s"$containerLogsPath/container-$containerName.log",
    "DETAILED_LOG_PATH"     -> "/dev/null",
    "WAVES_NODE_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_OPTS" -> List(
      "-Xmx1024M",
      s"-Djava.util.logging.config.file=$baseContainerPath/jul.properties",
      s"-Dlogback.configurationFile=$baseContainerPath/logback-container.xml",
      s"-Dlogback.brief.fullPath=$containerLogsPath/container-$containerName.log",
      s"-Dwaves.network.declared-address=$ip:6883"
    ).mkString(" ", " ", " ")
  )
}
