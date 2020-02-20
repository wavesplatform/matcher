package com.wavesplatform.dex.it.docker

import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}

import cats.Id
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.collections.Implicits.ListOps
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.resources.getRawContentFromResource
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.Network.NetworkImpl

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class DexContainer private (override val internalIp: String, underlying: GenericContainer)(
    implicit
    tryHttpBackend: LoggingSttpBackend[Try, Nothing],
    futureHttpBackend: LoggingSttpBackend[Future, Nothing],
    ec: ExecutionContext)
    extends BaseContainer(DexContainer.baseContainerPath, underlying) {

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(DexContainer.restApiPort))
  def restApiAddress: InetSocketAddress                                      = cachedRestApiAddress.get()

  override def api: DexApi[Id]          = fp.sync { DexApi[Try](apiKey, restApiAddress) }
  override def asyncApi: DexApi[Future] = DexApi[Future](apiKey, restApiAddress)
}

object DexContainer extends ScorexLogging {

  private val isProfilingEnabled: Boolean = Option(System.getenv("WAVES_DEX_PROFILING")).getOrElse("false").toBoolean
  private val baseContainerPath: String   = "/opt/waves-dex"
  private val containerLogsPath: String   = s"$baseContainerPath/logs"

  private val restApiPort: Int = 6886 // application.conf waves.dex.rest-api.port

  def apply(name: String,
            networkName: String,
            network: NetworkImpl,
            internalIp: String,
            runConfig: Config,
            suiteInitialConfig: Config,
            localLogsDir: Path)(implicit
                                tryHttpBackend: LoggingSttpBackend[Try, Nothing],
                                futureHttpBackend: LoggingSttpBackend[Future, Nothing],
                                ec: ExecutionContext): DexContainer = {

    val underlying = GenericContainer(
      dockerImage = "com.wavesplatform/dex-it:latest",
      exposedPorts = Seq(restApiPort),
      env = getEnv(name),
      waitStrategy = ignoreWaitStrategy
    ).configure { c =>
      c.withNetwork(network)
      c.withNetworkAliases("d3x")
      c.withFileSystemBind(localLogsDir.toString, containerLogsPath, BindMode.READ_WRITE)
      c.withCreateContainerCmdModifier {
        _.withName(s"$networkName-$name") // network.getName returns random id
          .withIpv4Address(internalIp): Unit
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
    "BRIEF_LOG_PATH"       -> s"$containerLogsPath/container-$containerName.log",
    "DETAILED_LOG_PATH"    -> s"$containerLogsPath/container-$containerName.detailed.log",
    "WAVES_DEX_CONFIGPATH" -> s"$baseContainerPath/$containerName.conf",
    "WAVES_DEX_OPTS" ->
      List(
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
        }
        .mkString(" ", " ", " ")
  )
}
