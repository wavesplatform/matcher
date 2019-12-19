package com.wavesplatform.dex.it.api.node

import java.net.InetSocketAddress

import cats.instances.try_._
import cats.{Functor, Id}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.dex.it.docker.base
import com.wavesplatform.dex.it.docker.base.BaseContainer
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.fp.CanExtract
import monix.eval.Coeval
import mouse.any._

import scala.util.Try

trait NewHasWavesNode { self: BaseContainersKit =>

  protected val wavesNodeContainer: Coeval[base.WavesNodeContainer] = Coeval.evalOnce { createWavesNode("waves-1") }

  protected val cachedWavesNodeApiAddress        = CachedData(wavesNodeContainer().restApiAddress)
  protected val cachedWavesNodeNetworkApiAddress = CachedData(wavesNodeContainer().networkAddress)

  protected def wavesNodeNetworkApiAddress: InetSocketAddress = cachedWavesNodeNetworkApiAddress.get()

  protected def wavesNodeApi: NodeApi[Id] = fp.sync { NodeApi[Try](apiKey, cachedWavesNodeApiAddress.get()) }

  protected implicit def toNodeExplicitGetOps[F[_]: Functor: CanExtract](self: NodeApi[F]): NodeApiOps.ExplicitGetNodeApiOps[F] = {
    new NodeApiOps.ExplicitGetNodeApiOps[F](self)
  }

  protected def wavesNodeInitialSuiteConfig: Config = ConfigFactory.empty()

  protected val wavesNodeRunConfig: Coeval[Config] = Coeval.evalOnce(GenesisConfig.config)

  protected def createWavesNode(name: String,
                                runConfig: Config = wavesNodeRunConfig(),
                                suiteInitialConfig: Config = wavesNodeInitialSuiteConfig): base.WavesNodeContainer = {
    BaseContainer.createWavesNodeContainer(name, runConfig, suiteInitialConfig) unsafeTap addKnownContainer
  }

  protected def invalidateWavesNodeCaches(): Unit = {
    cachedWavesNodeApiAddress.invalidate()
    cachedWavesNodeNetworkApiAddress.invalidate()
  }
}
