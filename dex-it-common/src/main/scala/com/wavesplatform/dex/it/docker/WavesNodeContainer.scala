package com.wavesplatform.dex.it.docker

import cats.tagless.FunctorK
import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.Transformations
import com.wavesplatform.dex.it.api.node.{AsyncEnrichedNodeApi, NodeApi}
import com.wavesplatform.dex.it.api.responses.node.ErrorResponse
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.resources.getRawContentFromResource
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import org.testcontainers.containers.{BindMode, Network}
import sttp.model.StatusCode

import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class WavesNodeContainer(override val internalIp: String, underlying: GenericContainer)(
  implicit
  tryHttpBackend: LoggingSttpBackend[Try, Any],
  futureHttpBackend: LoggingSttpBackend[Future, Any],
  ec: ExecutionContext
) extends BaseContainer(WavesNodeContainer.baseContainerPath, underlying) {

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(WavesNodeContainer.restApiPort))
  def restApiAddress: InetSocketAddress = cachedRestApiAddress.get()

  private val cachedNetworkAddress = CachedData(getInternalAddress(WavesNodeContainer.networkPort))
  private val cachedMatcherExtGrpcApiAddress = CachedData(getExternalAddress(WavesNodeContainer.matcherGrpcExtensionPort))
  private val cachedBlockchainUpdatesExtGrpcApiAddress = CachedData(getExternalAddress(WavesNodeContainer.blockchainUpdatesGrpcExtensionPort))

  def networkAddress: InetSocketAddress = cachedNetworkAddress.get()
  def matcherExtGrpcApiAddress: InetSocketAddress = cachedMatcherExtGrpcApiAddress.get()
  def blockchainUpdatesExtGrpcApiAddress: InetSocketAddress = cachedBlockchainUpdatesExtGrpcApiAddress.get()

  // This won't help after reconnect to the network, need to recreate clients
  def matcherExtApiTarget: String = s"${matcherExtGrpcApiAddress.getHostName}:${matcherExtGrpcApiAddress.getPort}"
  def blockchainUpdatesExtApiTarget: String = s"${blockchainUpdatesExtGrpcApiAddress.getHostName}:${blockchainUpdatesExtGrpcApiAddress.getPort}"

  private val apiFunctorK: FunctorK[NodeApi] = FunctorK[NodeApi] // IntelliJ FIX

  val tf = new Transformations[ErrorResponse]
  import tf._

  // See DexContainer about apis

  def api: NodeApi[SyncUnsafe] = apiFunctorK.mapK(asyncRawApi)(toSyncUnsafe)
  def tryApi: NodeApi[SyncTry] = apiFunctorK.mapK(asyncRawApi)(toSyncTry)
  def httpApi: NodeApi[SyncHttp] = apiFunctorK.mapK(asyncRawApi)(toSyncHttp)
  def rawApi: NodeApi[SyncRaw] = apiFunctorK.mapK(asyncRawApi)(toSyncRaw)

  def asyncApi: NodeApi[AsyncUnsafe] = apiFunctorK.mapK(asyncRawApi)(toAsyncUnsafe)
  def asyncTryApi: NodeApi[AsyncTry] = apiFunctorK.mapK(asyncRawApi)(toAsyncTry)
  def asyncRawApi: AsyncEnrichedNodeApi = new AsyncEnrichedNodeApi(apiKey, restApiAddress)

  override def waitReady(): Unit = {
    val r = Iterator
      .continually {
        Thread.sleep(1000)
        try httpApi.currentHeightOrig.code == StatusCode.Ok
        catch {
          case _: Throwable => false
        }
      }
      .take(60)
      .find(_ == true)

    if (!r.contains(true)) throw new RuntimeException(s"${underlying.containerId} is not ready, all attempts are out")
  }

  override def invalidateCaches(): Unit = {
    super.invalidateCaches()
    cachedNetworkAddress.invalidate()
    cachedMatcherExtGrpcApiAddress.invalidate()
    cachedBlockchainUpdatesExtGrpcApiAddress.invalidate()
  }

  override def printDebugMessage(text: String): Unit = asyncRawApi.print(text)
}

object WavesNodeContainer extends ScorexLogging {

  private val baseContainerPath: String = "/opt/waves"
  private val containerLogsPath: String = s"$baseContainerPath/logs"

  private val restApiPort: Int = 6869 // application.conf waves.rest-api.port
  private val networkPort: Int = 6863 // application.conf waves.network.port
  val matcherGrpcExtensionPort: Int = 6887 // application.conf waves.dex.grpc.integration.port
  val blockchainUpdatesGrpcExtensionPort: Int = 6881 // application.conf waves.dex.blockchain-updates-grpc.integration.port

  private val exposedPorts = List(restApiPort, networkPort, matcherGrpcExtensionPort, blockchainUpdatesGrpcExtensionPort)

  val wavesNodeNetAlias: String = "waves.nodes"

  def apply(
    name: String,
    networkName: String,
    network: Network,
    internalIp: String,
    runConfig: Config,
    suiteInitialConfig: Config,
    localLogsDir: Path,
    image: String,
    netAlias: Option[String] = Some(wavesNodeNetAlias)
  )(implicit
    tryHttpBackend: LoggingSttpBackend[Try, Any],
    futureHttpBackend: LoggingSttpBackend[Future, Any],
    ec: ExecutionContext
  ): WavesNodeContainer = {

    val underlying = GenericContainer(
      dockerImage = image,
      env = getEnv(name, internalIp),
      waitStrategy = ignoreWaitStrategy
    ).configure { c =>
      c.withNetwork(network)
      netAlias.foreach(c.withNetworkAliases(_))
      c.withFileSystemBind(localLogsDir.toString, containerLogsPath, BindMode.READ_WRITE)
      c.withCreateContainerCmdModifier { cmd =>
        cmd.withName(s"$networkName-$name") // network.getName returns random id
          .withIpv4Address(internalIp)
        cmd.getHostConfig.withPortBindings(PortBindingKeeper.getBindings(cmd, exposedPorts))
      }

      // Copy files to container
      List(
        ("waves-base.conf", getRawContentFromResource("nodes/waves-base.conf"), false),
        (s"$name.conf", getRawContentFromResource(s"nodes/$name.conf"), false),
        ("run.conf", runConfig.rendered, true),
        ("suite.conf", suiteInitialConfig.rendered, true),
        ("logback-container.xml", getRawContentFromResource("nodes/logback-container.xml"), false),
        ("jul.properties", getRawContentFromResource("nodes/jul.properties"), false),
        ("/lp/accounts", "", false)
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
    "BRIEF_LOG_PATH" -> s"$containerLogsPath/container-$containerName.log",
    "DETAILED_LOG_PATH" -> "/dev/null",
    "WAVES_NODE_DETAILED_LOG_PATH" -> "/dev/null", // Backward compatibility for v1.1.10+v2.0.3
    "WAVES_NODE_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_OPTS" -> List(
      "-Xmx1024M",
      s"-Djava.util.logging.config.file=$baseContainerPath/jul.properties",
      s"-Dlogback.configurationFile=$baseContainerPath/logback-container.xml",
      s"-Dlogback.brief.fullPath=$containerLogsPath/container-$containerName.log",
      s"-Dwaves.network.declared-address=$ip:6883",
      "-Dmonix.environment.batchSize=1" // TODO DEX-993 Prod node setting?
    ).mkString(" ", " ", " ")
  )

}
