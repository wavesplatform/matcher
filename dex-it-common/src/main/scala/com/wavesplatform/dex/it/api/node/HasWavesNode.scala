package com.wavesplatform.dex.it.api.node

import cats.{FlatMap, Functor}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.fp.CanRepeat
import mouse.any._

trait HasWavesNode { self: BaseContainersKit =>
  protected val defaultNodeImage = "wavesplatform/waves-integration-it:latest"
  private val nodeImage = Option(System.getenv("NODE_IMAGE")).getOrElse(defaultNodeImage)

  implicit protected def toNodeApiSyntax[F[_]: Functor: FlatMap: CanRepeat](self: NodeApi[F]): NodeApiSyntax.Ops[F] =
    new NodeApiSyntax.Ops[F](self)

  protected def wavesNodeInitialSuiteConfig: Config = ConfigFactory.empty()

  protected lazy val wavesNodeRunConfig: Config = GenesisConfig.config

  protected def createWavesNode(
    name: String,
    runConfig: Config = wavesNodeRunConfig,
    suiteInitialConfig: Config = wavesNodeInitialSuiteConfig,
    image: String = nodeImage,
    netAlias: Option[String] = Some(WavesNodeContainer.wavesNodeNetAlias),
    lpAccounts: Seq[PublicKey] = Seq.empty[PublicKey]
  ): WavesNodeContainer =
    WavesNodeContainer(
      name,
      networkName,
      network,
      getIp(name),
      runConfig,
      suiteInitialConfig,
      localLogsDir,
      image,
      netAlias,
      lpAccounts
    ) unsafeTap addKnownContainer

  protected lazy val wavesNode1: WavesNodeContainer = createWavesNode("waves-1")
}
