package com.wavesplatform.dex.it.docker.base

import java.net.InetSocketAddress

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
}
