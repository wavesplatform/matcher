package com.wavesplatform.it

import java.util.Properties
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.apache.kafka.clients.admin.{AdminClient, NewTopic}
import org.scalatest._
import org.scalatest.concurrent.Eventually

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(
    timeout = 1.minute,
    interval = 1.second
  )

  val smartFee         = 0.004.waves
  val minFee           = 0.001.waves + smartFee
  val issueFee         = 1.waves
  val smartIssueFee    = 1.waves + smartFee
  val leasingFee       = 0.002.waves + smartFee
  val tradeFee         = 0.003.waves
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  private val topicName = s"dex-${ThreadLocalRandom.current().nextInt(0, Int.MaxValue)}"

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/dex-it:latest",
    tag = getClass.getSimpleName,
    suiteConfig = baseConfig(topicName)
  )

  protected def node = dockerNodes().head

  protected def nodeConfigs: Seq[Config] = MatcherPriceAssetConfig.Configs

  override protected def beforeAll(): Unit = {
    // Hack because we haven't own Docker class
    Map(
      "org.asynchttpclient.keepAlive"       -> "false",
      "org.asynchttpclient.maxRequestRetry" -> "0",
      "org.asynchttpclient.readTimeout"     -> "120000",
      "org.asynchttpclient.requestTimeout"  -> "120000",
      "org.asynchttpclient.ioThreadsCount"  -> "15"
    ).foreach(Function.tupled(System.setProperty))

    createKafkaTopic(topicName)
    super.beforeAll()
  }

}

object MatcherSuiteBase {
  private def baseConfig(topicName: String): Config = kafkaServer.fold(ConfigFactory.empty()) { kafkaServer =>
    ConfigFactory.parseString(s"""
         |logback.configurationFile=/opt/waves/logback-container.xml
         |
         |waves.dex.events-queue {
         |  type = kafka
         |  kafka {
         |    servers = "$kafkaServer"
         |    topic = "$topicName"
         |  }
         |}""".stripMargin)
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
      val newTopic = new NewTopic(name, 0, 0.toShort)
      adminClient.createTopics(java.util.Collections.singletonList(newTopic))
    } finally {
      adminClient.close()
    }
  }

  private def kafkaServer: Option[String] = Option(System.getenv("KAFKA_SERVER"))
}
