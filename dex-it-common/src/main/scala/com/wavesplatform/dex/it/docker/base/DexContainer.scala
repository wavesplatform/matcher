package com.wavesplatform.dex.it.docker.base

import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}

import cats.Id
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import com.dimafeng.testcontainers.GenericContainer
import com.typesafe.config.Config
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.collections.Implicits.ListOps
import com.wavesplatform.dex.it.docker.Implicits.GenericContainerOps
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.resources.getRawContentFromResource
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.utils.ScorexLogging
import org.testcontainers.containers.BindMode
import org.testcontainers.containers.Network.NetworkImpl
import org.testcontainers.containers.wait.strategy.AbstractWaitStrategy

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class DexContainer private (underlying: GenericContainer)(implicit
                                                                     tryHttpBackend: LoggingSttpBackend[Try, Nothing],
                                                                     futureHttpBackend: LoggingSttpBackend[Future, Nothing],
                                                                     ec: ExecutionContext)
    extends BaseContainer(underlying) {

  override protected val baseContainerPath: String = DexContainer.baseContainerPath
  override protected val internalIp: String        = underlying.containerIpAddress // ???

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(DexContainer.restApiPort))

  override def api: DexApi[Id]          = fp.sync { DexApi[Try](BaseContainer.apiKey, cachedRestApiAddress.get()) }
  override def asyncApi: DexApi[Future] = DexApi[Future](BaseContainer.apiKey, cachedRestApiAddress.get())
}

object DexContainer extends ScorexLogging {

  val isProfilingEnabled: Boolean = Option(System.getenv("WAVES_DEX_PROFILING")).getOrElse("false").toBoolean
  val baseContainerPath: String   = "/opt/waves-dex"

  private val containerLogsPath: String                = s"$baseContainerPath/logs"
  private val ignoreWaitStrategy: AbstractWaitStrategy = () => ()

  val restApiPort: Int = 6886 // application.conf waves.dex.rest-api.port
  val netAlias: String = "d3x"

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
      c.withNetworkAliases(netAlias)
      c.withFileSystemBind(localLogsDir.toFile.getAbsolutePath, containerLogsPath, BindMode.READ_WRITE)
      c.withCreateContainerCmdModifier {
        _.withName(s"$networkName-$name") // network.getName returns random id
          .withIpv4Address(internalIp): Unit
      }

      // Copy files to container
      List(
        ("dex-base.conf", getRawContentFromResource(s"dex-servers/dex-base.conf"), false),
        (s"$name.conf", getRawContentFromResource(s"dex-servers/$name.conf"), false),
        ("run.conf", runConfig.resolve().root().render(), true),
        ("suite.conf", suiteInitialConfig.resolve().root().render(), true),
        ("/doc/logback-container.xml", getRawContentFromResource("dex-servers/logback-container.xml"), false)
      ).map {
        case (fileName, content, logContent) =>
          val containerPath = Paths.get(baseContainerPath, fileName).toString
          log.trace(s"[name=$name] Write to '$containerPath'${if (logContent) s":\n$content" else ""}")
          c.withCopyFileToContainer(containerPath, content)
      }
    }

    DexContainer(underlying)
  }

  private def getEnv(containerName: String): Map[String, String] = Map(
    "WAVES_DEX_DETAILED_LOG_PATH" -> s"$containerLogsPath/container-$containerName.detailed.log",
    "WAVES_DEX_CONFIGPATH"        -> s"$baseContainerPath/$containerName.conf",
    "WAVES_DEX_OPTS" ->
      List(
        "-J-Xmx1024M",
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
