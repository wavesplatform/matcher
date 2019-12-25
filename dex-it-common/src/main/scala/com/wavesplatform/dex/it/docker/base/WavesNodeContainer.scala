package com.wavesplatform.dex.it.docker.base

import java.net.InetSocketAddress

import cats.Id
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import com.dimafeng.testcontainers.GenericContainer
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.dex.it.api.node.NodeApi
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.docker.base.info.WavesNodeContainerInfo
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

final case class WavesNodeContainer(name: String, underlying: GenericContainer)(implicit
                                                                                tryHttpBackend: LoggingSttpBackend[Try, Nothing],
                                                                                futureHttpBackend: LoggingSttpBackend[Future, Nothing],
                                                                                ec: ExecutionContext)
    extends BaseContainer(underlying, WavesNodeContainerInfo) {

  override protected val cachedRestApiAddress: CachedData[InetSocketAddress] = CachedData(getExternalAddress(WavesNodeContainerInfo.restApiPort))

  private val cachedNetworkAddress = CachedData(getInternalAddress(WavesNodeContainerInfo.networkPort))
  private val cachedGrpcApiAddress = CachedData(getExternalAddress(WavesNodeContainerInfo.dexGrpcExtensionPort))

  def networkAddress: InetSocketAddress = cachedNetworkAddress.get()
  def grpcApiAddress: InetSocketAddress = cachedGrpcApiAddress.get()

  def grpcApiTarget: String = s"${grpcApiAddress.getHostName}:${grpcApiAddress.getPort}"

  override def api: NodeApi[Id]               = fp.sync { NodeApi[Try](BaseContainer.apiKey, cachedRestApiAddress.get()) }
  override def asyncApi: HasWaitReady[Future] = NodeApi[Future](BaseContainer.apiKey, cachedRestApiAddress.get())

  override def invalidateCaches(): Unit = {
    super.invalidateCaches()
    cachedNetworkAddress.invalidate()
    cachedGrpcApiAddress.invalidate()
  }
}
