package com.wavesplatform.dex.it.docker.base

import java.net.InetSocketAddress
import java.nio.file.{Path, Paths}

import cats.Id
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import com.dimafeng.testcontainers.GenericContainer
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.docker.base.info.DexContainerInfo
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class DexContainer(name: String, underlying: GenericContainer)(implicit
                                                                          tryHttpBackend: LoggingSttpBackend[Try, Nothing],
                                                                          futureHttpBackend: LoggingSttpBackend[Future, Nothing],
                                                                          ec: ExecutionContext)
    extends BaseContainer(underlying, DexContainerInfo) {

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(DexContainerInfo.restApiPort))

  override def api: DexApi[Id]          = fp.sync { DexApi[Try](BaseContainer.apiKey, cachedRestApiAddress.get()) }
  override def asyncApi: DexApi[Future] = DexApi[Future](BaseContainer.apiKey, cachedRestApiAddress.get())

  override protected def saveSystemLogs(localLogsDir: Path): Unit = {

    val containerSystemLogPath = Paths.get(DexContainerInfo.baseContainerPath, "system.log")
    val localSystemLogPath     = localLogsDir.resolve(s"container-$name.system.log")

    log.info(s"$prefix Loading system log from '$containerSystemLogPath' to '$localSystemLogPath'")
    copyFileToLocalPath(containerSystemLogPath, localSystemLogPath)

    // TODO
    if (BaseContainer.isProfilingEnabled) {
      val containerProfilerLogPath = Paths.get(DexContainerInfo.baseContainerPath, s"$name.snapshot")
      val localProfilerLogPath = localLogsDir.resolve(s"container-$name.snapshot")
      log.info(s"$prefix Loading profiler log from '$containerProfilerLogPath' to '$localProfilerLogPath'")
      copyFileToLocalPath(containerProfilerLogPath, localProfilerLogPath)
    }
  }
}
