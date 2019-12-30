package com.wavesplatform.dex.it.api

import java.lang
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.{ConcurrentHashMap, Executors}

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp.TryHttpURLConnectionBackend
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend
import com.wavesplatform.dex.it.docker.base.BaseContainer
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import mouse.any._
import org.asynchttpclient.DefaultAsyncHttpClientConfig

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait BaseContainersKit extends ScorexLogging {

  protected val moduleName: String

  protected val knownContainers: ConcurrentHashMap.KeySetView[BaseContainer, lang.Boolean] = ConcurrentHashMap.newKeySet[BaseContainer]()

  protected def addKnownContainer(container: BaseContainer): Unit = knownContainers.add(container)
  protected def forgetContainer(container: BaseContainer): Unit   = knownContainers.remove(container)

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

  protected implicit val tryHttpBackend: LoggingSttpBackend[Try, Nothing] = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  /** A location for logs from containers on local machine */
  protected implicit val localLogsDir: Coeval[Path] = Coeval.evalOnce {

    val runId: String = Option { System.getenv("RUN_ID") } getOrElse DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss").format(LocalDateTime.now)

    Option { System.getProperty("waves.it.logging.dir") }
      .map { Paths get _ }
      .getOrElse {
        Paths.get(System.getProperty("user.dir"), moduleName, "target", "logs", runId, getClass.getSimpleName.replaceAll("""(\w)\w*\.""", "$1."))
      } unsafeTap { Files.createDirectories(_) }
  }

  protected def stopBaseContainers(): Unit = {
    log.debug("Stopping containers")
    knownContainers.forEach { _.stopAndSaveLogs(withRemoving = true) }
    futureHttpBackend.close()
    tryHttpBackend.close()
  }
}
