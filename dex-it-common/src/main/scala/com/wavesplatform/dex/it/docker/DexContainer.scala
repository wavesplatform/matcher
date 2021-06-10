package com.wavesplatform.dex.it.docker

import cats.Functor
import cats.tagless.FunctorK
import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.app.MatcherStatus.Working
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.it.api._
import com.wavesplatform.dex.it.api.dex.{AsyncEnrichedDexApi, DexApi, DexApiSyntax}
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.collections.Implicits.ListOps
import com.wavesplatform.dex.it.fp.CanRepeat
import com.wavesplatform.dex.it.resources.getRawContentFromResource
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.Network.NetworkImpl

import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class DexContainer private (override val internalIp: String, underlying: GenericContainer)(
  implicit
  tryHttpBackend: LoggingSttpBackend[Try, Any],
  futureHttpBackend: LoggingSttpBackend[Future, Any],
  ec: ExecutionContext
) extends BaseContainer(DexContainer.baseContainerPath, underlying) {

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(DexContainer.restApiPort))
  def restApiAddress: InetSocketAddress = cachedRestApiAddress.get()

  private val apiFunctorK: FunctorK[DexApi] = FunctorK[DexApi] // IntelliJ FIX

  val tf = new Transformations[MatcherError]
  import tf._

  /*
  Which api to use? Consider a lesser power principle:
  1. Least power: api - we can get an entity, httpApi - we can get an HTTP response only
  2. Moderate power: tryApi - we can get either an entity or a domain error (MatcherError). But we need to write more code
  3. The most powerful: rawApi - we can get all information, including HTTP response. This method is more powerful, but requires more code

  Use async* only if you want to do parallel requests.
   */

  def api: DexApi[SyncUnsafe] = apiFunctorK.mapK(asyncRawApi)(toSyncUnsafe)
  def tryApi: DexApi[SyncTry] = apiFunctorK.mapK(asyncRawApi)(toSyncTry)
  def httpApi: DexApi[SyncHttp] = apiFunctorK.mapK(asyncRawApi)(toSyncHttp)
  def rawApi: DexApi[SyncRaw] = apiFunctorK.mapK(asyncRawApi)(toSyncRaw)

  def asyncApi: DexApi[AsyncUnsafe] = apiFunctorK.mapK(asyncRawApi)(toAsyncUnsafe)
  def asyncTryApi: DexApi[AsyncTry] = apiFunctorK.mapK(asyncRawApi)(toAsyncTry)
  def asyncRawApi: AsyncEnrichedDexApi = new AsyncEnrichedDexApi(apiKey, restApiAddress)

  implicit protected def toDexApiSyntax[F[_]: Functor: CanRepeat](self: DexApi[F]): DexApiSyntax.Ops[F] =
    new DexApiSyntax.Ops[F](self)

  def saveSnapshotAndWait(): Unit = {
    val currentOffset = api.getCurrentOffset
    api.saveSnapshots
    api.waitForOldestSnapshotOffset(_ >= currentOffset)
  }

  /**
   * Use this method to restart Matcher with a new config.
   * @see restartWithNewSuiteConfig
   */
  def safeRestartWithNewSuiteConfig(newSuiteConfig: Config): Unit = {
    saveSnapshotAndWait()
    super.restartWithNewSuiteConfig(newSuiteConfig)
  }

  /**
   * This method could affect an execution of orders. If you provide such settings, use safe.
   * Use on your risk.
   */
  override def restartWithNewSuiteConfig(newSuiteConfig: Config): Unit =
    super.restartWithNewSuiteConfig(newSuiteConfig)

  override def waitReady(): Unit = {
    val r = Iterator
      .continually {
        Thread.sleep(1000)
        try {
          val s = api.getSystemStatus
          s.blockchain.isInstanceOf[Status.Working] && s.service == Working
        } catch {
          case _: Throwable => false
        }
      }
      .take(60)
      .find(_ == true)

    if (!r.contains(true)) throw new RuntimeException(s"${underlying.containerId} is not ready, all attempts are out")
  }

  override def printDebugMessage(text: String): Unit = asyncRawApi.print(text)
}

object DexContainer extends ScorexLogging {

  private val isProfilingEnabled: Boolean = Option(System.getenv("WAVES_DEX_PROFILING")).getOrElse("false").toBoolean
  private val baseContainerPath: String = "/usr/share/waves-dex"
  private val containerLogsPath: String = s"$baseContainerPath/logs"

  private val restApiPort: Int = 6886 // application.conf waves.dex.rest-api.port
  private val exposedPorts = Seq(6886)

  def apply(
    name: String,
    networkName: String,
    network: NetworkImpl,
    internalIp: String,
    runConfig: Config,
    suiteInitialConfig: Config,
    localLogsDir: Path,
    image: String
  )(implicit
    tryHttpBackend: LoggingSttpBackend[Try, Any],
    futureHttpBackend: LoggingSttpBackend[Future, Any],
    ec: ExecutionContext
  ): DexContainer = {

    val underlying = GenericContainer(
      dockerImage = image,
      env = getEnv(name),
      waitStrategy = ignoreWaitStrategy
    ).configure { c =>
      c.withNetwork(network)
      c.withFileSystemBind(localLogsDir.toString, containerLogsPath, BindMode.READ_WRITE)
      c.withCreateContainerCmdModifier { cmd =>
        cmd.withName(s"$networkName-$name") // network.getName returns random id
          .withIpv4Address(internalIp)
        cmd.getHostConfig.withPortBindings(PortBindingKeeper.getBindings(cmd, exposedPorts))
      }

      // Copy files to container
      List(
        ("dex-base.conf", getRawContentFromResource(s"dex-servers/dex-base.conf"), false),
        (s"$name.conf", getRawContentFromResource(s"dex-servers/$name.conf"), false),
        ("run.conf", runConfig.rendered, true),
        ("suite.conf", suiteInitialConfig.rendered, true),
        ("/doc/logback-container.xml", getRawContentFromResource("dex-servers/logback-container.xml"), false),
        ("jul.properties", getRawContentFromResource("dex-servers/jul.properties"), false)
      ).map {
        case (fileName, content, logContent) =>
          val containerPath = Paths.get(baseContainerPath, fileName).toString
          log.trace(s"[name=$name] Write to '$containerPath'${if (logContent) s":\n$content" else ""}")
          c.withCopyFileToContainer(MountableFileOps.fromContent(content), containerPath)
      }
    }

    DexContainer(internalIp, underlying)
  }

  private def getEnv(containerName: String): Map[String, String] = Map(
    "BRIEF_LOG_PATH" -> s"$containerLogsPath/container-$containerName.log",
    "DETAILED_LOG_PATH" -> s"$containerLogsPath/container-$containerName.detailed.log",
    "WAVES_DEX_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_DEX_DETAILED_LOG_PATH" -> s"$containerLogsPath/container-$containerName.detailed.log", // Backward compatibility for v2.0.3
    "WAVES_DEX_OPTS" -> List(
      "-J-Xmx1024M",
      s"-Djava.util.logging.config.file=$baseContainerPath/jul.properties",
      "-Dlogback.stdout.enabled=false",
      "-Dlogback.file.enabled=false",
      s"-Dlogback.configurationFile=$baseContainerPath/doc/logback.xml",
      s"-Dlogback.include.file=$baseContainerPath/doc/logback-container.xml",
      s"-Dlogback.brief.fullPath=$containerLogsPath/container-$containerName.log",
      s"-Dlogback.detailed.fullPath=$containerLogsPath/container-$containerName.detailed.log"
    ).prependIf(isProfilingEnabled) {
      // https://www.yourkit.com/docs/java/help/startup_options.jsp
      s"-J-agentpath:/usr/local/YourKit-JavaProfiler-2019.8/bin/linux-x86-64/libyjpagent.so=port=10001,listen=all" +
      s",sampling,monitors,sessionname=prof-$containerName,snapshot_name_format={sessionname}," +
      s"dir=$containerLogsPath,logdir=$containerLogsPath,onexit=snapshot"
    }.mkString(" ", " ", " ")
  )

  /**
   * @param resolve A relate to the base directory path of application
   * @note Works only with /
   */
  def containerPath(resolve: String): String = s"$baseContainerPath/$resolve"
}
