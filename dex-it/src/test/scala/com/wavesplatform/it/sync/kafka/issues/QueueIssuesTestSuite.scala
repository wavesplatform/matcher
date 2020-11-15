package com.wavesplatform.it.sync.kafka.issues

import java.util.Collections
import java.util.concurrent.ThreadLocalRandom

import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.it.WsSuiteBase
import org.apache.kafka.clients.admin.{AlterConfigOp, ConfigEntry}
import org.apache.kafka.common.config.{ConfigResource, TopicConfig}

import scala.concurrent.duration.DurationInt

class QueueIssuesTestSuite extends WsSuiteBase with HasKafka {

  private val requestTimeout = 15.seconds
  private val maxFailures = 5

  // Hacks, see DEX-794
  private val deliveryTimeout = requestTimeout + 1.second

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex {
  price-assets = [ "$UsdId", "WAVES" ]
  events-queue {
    kafka.producer.client {
      acks = 1
      request.timeout.ms = ${requestTimeout.toMillis}
      delivery.timeout.ms = ${deliveryTimeout.toMillis}
      connections.max.idle.ms = ${deliveryTimeout.toMillis}
    }

    circuit-breaker {
      max-failures = $maxFailures
      reset-timeout = 2000ms
    }
  }
}""")

  private val topicName = s"test-${ThreadLocalRandom.current.nextInt(0, Int.MaxValue)}"
  override protected lazy val dexRunConfig = dexKafkaConfig(topicName).withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    kafka.start()

    broadcastAndAwait(IssueUsdTx)
    createKafkaTopic(topicName, Some(kafka.bootstrapServers))
    dex1.start()
  }

  "Matcher should stop working with appropriate error if the queue was " - {
    "cleared" in {
      fillQueueAndSaveSnapshots()

      dex1.replaceSuiteConfig(
        ConfigFactory
          .parseString("""waves.dex {
  events-queue.kafka.topic = cleared-topic
  snapshots-interval = 3
}""").withFallback(dexInitialSuiteConfig)
      )
      dex1.stopWithoutRemove()
      clearTopic(topicName)

      try {
        dex1.start()
        fail("Expected Matcher stopped with the exit code of 12")
      } catch {
        case _: Throwable =>
          dex1.getState().getExitCodeLong shouldBe 12 // RecoveryError.code
      } finally {
        dex1.stop()
        dex1.start()
      }
    }

    "switched" in {
      fillQueueAndSaveSnapshots()

      try {
        dex1.restartWithNewSuiteConfig(
          ConfigFactory
            .parseString("""waves.dex {
  events-queue.kafka.topic = new-topic
  snapshots-interval = 3
}""").withFallback(dexInitialSuiteConfig)
        )
        fail("Expected Matcher stopped with the exit code of 12")
      } catch {
        case _: Throwable =>
          dex1.getState().getExitCodeLong shouldBe 12 // RecoveryError.code
      } // Add finally (above) if you write a new test
    }
  }

  private def clearTopic(topicName: String): Unit = {
    val kafkaClient = mkKafkaAdminClient(s"$kafkaIp:9092")
    try {
      val resource = new ConfigResource(ConfigResource.Type.TOPIC, topicName)
      val retentionEntry = new ConfigEntry(TopicConfig.RETENTION_MS_CONFIG, "100") // Small retention to clear topic
      val alterRetentionOp = new AlterConfigOp(retentionEntry, AlterConfigOp.OpType.SET)

      val updateConfig = new java.util.HashMap[ConfigResource, java.util.Collection[AlterConfigOp]]()
      updateConfig.put(resource, Collections.singletonList(alterRetentionOp))

      kafkaClient.incrementalAlterConfigs(updateConfig)
    } finally kafkaClient.close()
  }

  private def fillQueueAndSaveSnapshots(): Unit = {
    val offsetBefore = dex1.api.lastOffset

    val orders = (1 to 10).map(i => mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 3.0, ttl = i.days))
    orders.foreach(dex1.api.place)
    orders.foreach(dex1.api.waitForOrderStatus(_, Status.Accepted))

    orders.foreach(dex1.api.cancel(alice, _))
    dex1.api.waitForOrderHistory(alice, true.some)(_.isEmpty)

    dex1.api.saveSnapshots
    eventually {
      dex1.api.allSnapshotOffsets(wavesUsdPair) shouldBe (offsetBefore + 20) // 10 orders * 2 (place and cancel)
    }
  }

}
