package com.wavesplatform.dex.it.api

import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference

import com.github.dockerjava.api.command.CreateNetworkCmd
import com.github.dockerjava.api.model.Network.Ipam
import com.google.common.primitives.Ints.toByteArray
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp.TryHttpURLConnectionBackend
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.docker.BaseContainer
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import mouse.any._
import org.asynchttpclient.DefaultAsyncHttpClientConfig
import org.testcontainers.containers.Network
import org.testcontainers.containers.Network.NetworkImpl

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Try}

trait BaseContainersKit extends ScorexLogging {

  protected val moduleName: String

  private val networkSeed   = Random.nextInt(0x100000) << 4 | 0x0A000000                                 // a random network in 10.x.x.x range
  private val networkPrefix = s"${InetAddress.getByAddress(toByteArray(networkSeed)).getHostAddress}/28" // 10.x.x.x/28 network will accommodate up to 13 nodes

  protected val networkName = s"waves-${Random.nextInt(Int.MaxValue)}"

  protected val network: NetworkImpl =
    Network
      .builder()
      .createNetworkCmdModifier { cmd: CreateNetworkCmd =>
        cmd.withIpam(new Ipam().withConfig(new Ipam.Config().withSubnet(networkPrefix).withIpRange(networkPrefix).withGateway(getIp(0xE))))
        cmd.withName(networkName)
      }
      .build()

  protected def getIp(name: String): String         = getIp(getNumber(name))
  protected def getIp(containerNumber: Int): String = InetAddress.getByAddress(toByteArray(containerNumber & 0xF | networkSeed)).getHostAddress
  protected def getNumber(name: String): Int = {
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

  // There is a List to remove containers in a reversed order
  protected val knownContainers = new AtomicReference[List[BaseContainer]](List.empty)

  protected def addKnownContainer(container: BaseContainer): Unit = knownContainers.updateAndGet(container :: _)

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor {
    Executors.newFixedThreadPool(10, new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build)
  }

  protected implicit val futureHttpBackend: LoggingSttpBackend[Future, Nothing] = new LoggingSttpBackend[Future, Nothing](
    AsyncHttpClientFutureBackend.usingConfig(
      new DefaultAsyncHttpClientConfig.Builder()
        .setMaxRequestRetry(0)
        .setReadTimeout(10000)
        .setKeepAlive(false)
        .setRequestTimeout(10000)
        .setIoThreadsCount(5)
        .build()
    )
  )

  protected implicit val tryHttpBackend: LoggingSttpBackend[Try, Nothing] = new LoggingSttpBackend[Try, Nothing](
    TryHttpURLConnectionBackend(customizeConnection = conn => {
      // For tests with a high latency
      conn.setConnectTimeout(30000)
      conn.setReadTimeout(30000)

      // This block of code to figh caches. It seems this doesn't help on macOS, but works on CI
      conn.setDefaultUseCaches(false)
      conn.setUseCaches(false)
      conn.setRequestProperty("Cache-Control", "no-store")
      conn.setRequestProperty("Pragma", "no-cache")
      conn.setRequestProperty("If-Modified-Since", "Sat, 1 Jan 2000 00:00:00 GMT")
      conn.setRequestProperty("Expired", "0")

      if (conn.getRequestMethod == "POST" && conn.getDoOutput) conn.setChunkedStreamingMode(0)
    })
  )

  /** A location for logs from containers on local machine */
  protected lazy val localLogsDir: Path = {
    val runId: String = Option { System.getenv("RUN_ID") } getOrElse DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss").format(LocalDateTime.now)
    def defaultDir =
      Paths.get(System.getProperty("user.dir"), moduleName, "target", "logs", runId, getClass.getSimpleName.replaceAll("""(\w)\w*\.""", "$1."))

    Option { System.getProperty("waves.it.logging.dir") }
      .map { Paths get _ }
      .getOrElse { defaultDir }
      .unsafeTap { Files.createDirectories(_) }
  }

  protected def stopBaseContainers(): Unit = {
    log.debug("Stopping containers")
    futureHttpBackend.close()
    tryHttpBackend.close()
    knownContainers.getAndUpdate(_ => List.empty).foreach(_.stopWithoutRemove()) // Graceful shutdown to save logs
  }
}
