package com.wavesplatform.dex.it.api.dex

import java.util.Properties
import java.util.concurrent.ThreadLocalRandom

import cats.Functor
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.docker.base.info.DexContainerInfo
import com.wavesplatform.dex.it.docker.base.{BaseContainer, DexContainer}
import com.wavesplatform.dex.it.fp.CanExtract
import mouse.any._
import org.apache.kafka.clients.admin.{AdminClient, NewTopic}
import org.testcontainers.containers.BindMode

import scala.collection.JavaConverters._

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
    val baseContainer = BaseContainer.create(DexContainerInfo)(name, runConfig, suiteInitialConfig)
    baseContainer.configure(_.withFileSystemBind(localLogsDir.toFile.getAbsolutePath, DexContainerInfo.containerLogsPath, BindMode.READ_WRITE))
    DexContainer(name, baseContainer) unsafeTap addKnownContainer
  }

  lazy val dex1: DexContainer = createDex("dex-1")

  protected def createKafkaTopic(name: String): Unit = kafkaServer.foreach { server =>
    val properties = new Properties()
    properties.putAll(
      Map(
        "bootstrap.servers"  -> server,
        "group.id"           -> s"create-$name",
        "key.deserializer"   -> "org.apache.kafka.common.serialization.StringDeserializer",
        "value.deserializer" -> "org.apache.kafka.common.serialization.StringDeserializer"
      ).asJava)

    val adminClient = AdminClient.create(properties)
    try {
      val newTopic = new NewTopic(name, 1, 1.toShort)
      adminClient.createTopics(java.util.Collections.singletonList(newTopic))
    } finally {
      adminClient.close()
    }
  }

  protected def kafkaServer: Option[String] = Option(System.getenv("KAFKA_SERVER"))
}
