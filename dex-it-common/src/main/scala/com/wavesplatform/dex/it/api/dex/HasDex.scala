package com.wavesplatform.dex.it.api.dex

import java.util.concurrent.ThreadLocalRandom

import cats.Functor
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.docker.base.info.DexContainerInfo
import com.wavesplatform.dex.it.docker.base.{BaseContainer, DexContainer}
import com.wavesplatform.dex.it.fp.CanExtract
import mouse.any._

trait HasDex { self: BaseContainersKit =>

  protected implicit def toDexExplicitGetOps[F[_]: CanExtract: Functor](self: DexApi[F]): DexApiOps.ExplicitGetDexApiOps[F] = {
    new DexApiOps.ExplicitGetDexApiOps[F](self)
  }

  protected def dexInitialSuiteConfig: Config = ConfigFactory.empty()

  protected lazy val dexRunConfig: Config = dexQueueConfig(ThreadLocalRandom.current.nextInt(0, Int.MaxValue))

  protected def dexQueueConfig(queueId: Int): Config = {
    Option { System.getenv("KAFKA_SERVER") }.fold { ConfigFactory.empty() } { kafkaServer =>
      ConfigFactory.parseString(s"""waves.dex.events-queue {
                                   |  type = kafka
                                   |  kafka {
                                   |    servers = "$kafkaServer"
                                   |    topic = "dex-$queueId"
                                   |  }
                                   |}""".stripMargin)
    }
  }

  protected def createDex(name: String, runConfig: Config = dexRunConfig, suiteInitialConfig: Config = dexInitialSuiteConfig): DexContainer = {
    DexContainer(name, BaseContainer.create(DexContainerInfo)(name, runConfig, suiteInitialConfig)) unsafeTap addKnownContainer
  }

  lazy val dex1: DexContainer = createDex("dex-1")
}
