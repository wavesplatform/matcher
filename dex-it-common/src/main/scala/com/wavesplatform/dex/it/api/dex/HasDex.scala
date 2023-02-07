package com.wavesplatform.dex.it.api.dex

import cats.Functor
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.dex.it.fp.CanRepeat
import mouse.any._
import org.apache.kafka.clients.admin.{AdminClient, NewTopic}

import java.util.Properties
import java.util.concurrent.ThreadLocalRandom
import scala.jdk.CollectionConverters._

trait HasDex { self: BaseContainersKit =>
  protected val defaultDexImage = "wavesplatform/dex-it:latest"
  private val dexImage = Option(System.getenv("DEX_IMAGE")).getOrElse(defaultDexImage)

  implicit protected def toDexApiSyntax[F[_]: Functor: CanRepeat](self: DexApi[F]): DexApiSyntax.Ops[F] =
    new DexApiSyntax.Ops[F](self)

  protected def dexInitialSuiteConfig: Config = ConfigFactory.empty()

  protected val kafkaTopicName = s"dex-${ThreadLocalRandom.current.nextInt(0, Int.MaxValue)}"

  protected lazy val dexRunConfig: Config = kafkaServer.fold(ConfigFactory.empty())(dexQueueConfig(_, kafkaTopicName))

  protected def kafkaServer: Option[String] = Option(System.getenv("KAFKA_SERVER"))

  protected def dexQueueConfig(kafkaServer: String, topicName: String): Config = ConfigFactory.parseString(
    s"""waves.dex.events-queue {
       |  type = kafka
       |  kafka {
       |    servers = "$kafkaServer"
       |    topic = "$topicName"
       |  }
       |}""".stripMargin
  )

  protected def createDex(
    name: String,
    runConfig: Config = dexRunConfig,
    suiteInitialConfig: Config = dexInitialSuiteConfig,
    image: String = dexImage,
    lpAccounts: Seq[PublicKey] = Seq.empty[PublicKey]
  ): DexContainer =
    DexContainer(
      name,
      networkName,
      network,
      getIp(name),
      runConfig,
      suiteInitialConfig,
      localLogsDir,
      image,
      lpAccounts
    ) unsafeTap addKnownContainer

  protected lazy val dex1: DexContainer = createDex("dex-1")

  protected def createKafkaTopic(name: String, server: Option[String] = kafkaServer): Unit = server.foreach { server =>
    val adminClient = mkKafkaAdminClient(server)
    try {
      val newTopic = new NewTopic(name, 1, 1.toShort)
      adminClient.createTopics(java.util.Collections.singletonList(newTopic))
    } finally adminClient.close()
  }

  protected def mkKafkaAdminClient(kafkaServer: String): AdminClient = {
    val properties = new Properties()
    properties.putAll(
      Map(
        "bootstrap.servers" -> kafkaServer,
        "key.deserializer" -> "org.apache.kafka.common.serialization.StringDeserializer",
        "value.deserializer" -> "org.apache.kafka.common.serialization.StringDeserializer"
      ).asJava
    )

    AdminClient.create(properties)
  }

}
