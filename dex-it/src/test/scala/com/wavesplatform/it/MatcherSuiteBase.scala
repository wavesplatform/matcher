package com.wavesplatform.it

import java.util.Properties

import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.api.dex.HasDex
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.dex.it.assets.DoubleOps
import com.wavesplatform.dex.it.config.{GenesisConfig, PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.it.matchers.ItMatchers
import com.wavesplatform.dex.it.test.InformativeTestStart
import com.wavesplatform.dex.it.waves.{MkWavesEntities, WavesFeeConstants}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.it.api.ApiExtensions
import com.wavesplatform.utils.ScorexLogging
import org.apache.kafka.clients.admin.{AdminClient, NewTopic}
import org.scalatest._
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration.DurationInt
import scala.collection.JavaConverters._

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with BaseContainersKit
    with HasDex
    with HasWavesNode
    with MkWavesEntities
    with ApiExtensions
    with ItMatchers
    with DoubleOps
    with WavesFeeConstants
    with PredefinedAssets
    with PredefinedAccounts
    with DiffMatcherWithImplicits
    with InformativeTestStart
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  override protected val moduleName: String = "dex-it"

  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(timeout = 30.seconds, interval = 1.second)

  override protected def beforeAll(): Unit = {
    log.debug(s"Perform beforeAll")
    kafkaServer.foreach { _ =>
      createKafkaTopic(dexRunConfig.getString("waves.dex.events-queue.kafka.topic"))
    }
    wavesNode1.start()
    dex1.start()
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Perform afterAll")
    stopBaseContainers()
    super.afterAll()
  }

  private def createKafkaTopic(name: String): Unit = kafkaServer.foreach { server =>
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

  private def kafkaServer: Option[String] = Option(System.getenv("KAFKA_SERVER"))
}
