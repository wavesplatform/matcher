package com.wavesplatform.dex.it.api.dex

import java.util.concurrent.ThreadLocalRandom

import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import cats.{Functor, Id}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.docker.base.{BaseContainer, DEXContainer}
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.fp.CanExtract
import monix.eval.Coeval
import mouse.any._

import scala.concurrent.Future
import scala.util.Try

trait HasDEX { self: BaseContainersKit =>

  protected val dexContainer: Coeval[DEXContainer] = Coeval.evalOnce { createDEX("dex-1") }

  protected val cachedDEXApiAddress = CachedData(dexContainer().restApiAddress)

  protected def dexApi: DexApi[Id]          = fp.sync { DexApi[Try](apiKey, cachedDEXApiAddress.get()) }
  protected def dexAsyncApi: DexApi[Future] = DexApi[Future](apiKey, cachedDEXApiAddress.get())

  protected implicit def toDEXExplicitGetOps[F[_]: CanExtract: Functor](self: DexApi[F]): DexApiOps.ExplicitGetDexApiOps[F] = {
    new DexApiOps.ExplicitGetDexApiOps[F](self)
  }

  protected def dexInitialSuiteConfig: Config = ConfigFactory.empty()

  protected val dexRunConfig: Coeval[Config] = Coeval.evalOnce { dexQueueConfig(ThreadLocalRandom.current.nextInt(0, Int.MaxValue)) }

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

  protected def createDEX(name: String, runConfig: Config = dexRunConfig(), suiteInitialConfig: Config = dexInitialSuiteConfig): DEXContainer = {
    BaseContainer.createDEXContainer(name, runConfig, suiteInitialConfig) unsafeTap addKnownContainer
  }

  protected def invalidateDEXCaches(): Unit = {
    cachedDEXApiAddress.invalidate()
  }
}
