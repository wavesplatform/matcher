package com.wavesplatform.it

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase.baseConfig
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import org.scalatest._
import org.scalatest.concurrent.Eventually

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

  protected override def createDocker: Docker = new Docker(
    imageName = "com.wavesplatform/dex-it:latest",
    tag = getClass.getSimpleName,
    suiteConfig = baseConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
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
    super.beforeAll()
  }
}

object MatcherSuiteBase {
  private def baseConfig(seed: Int): Config = Option(System.getenv("KAFKA_SERVER")).fold(ConfigFactory.empty()) { kafkaServer =>
    ConfigFactory.parseString(s"""
         |logback.configurationFile=/opt/waves/logback-container.xml
         |
         |waves.dex.events-queue {
         |  type = kafka
         |  kafka {
         |    servers = "$kafkaServer"
         |    topic = "dex-$seed"
         |  }
         |}""".stripMargin)
  }
}
