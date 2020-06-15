package tools

import java.io.File

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.queue.KafkaMatcherQueue.eventDeserializer
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.settings.toConfigOps
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

object KafkaTopicInfo extends App {
  implicit val system: ActorSystem = ActorSystem()

  val configFile = new File(args(0))
  val topic      = args(1)
  val from       = args(2).toLong
  val max        = args(3).toInt

  println(s"""configFile: ${configFile.getAbsolutePath}
             |topic: $topic
             |from: $from
             |max: $max""".stripMargin)

  val requestTimeout = java.time.Duration.ofNanos(5.seconds.toNanos)

  val config = ConfigFactory
    .parseString("""waves.dex.events-queue.kafka.consumer.client {
                   |  client.id = "kafka-topics-info"
                   |  enable.auto.commit = false
                   |  auto.offset.reset = earliest
                   |}
                   |
                   |""".stripMargin)
    .withFallback {
      ConfigFactory
        .parseFile(configFile)
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve()
        .getConfig("waves.dex.events-queue.kafka")
    }

  val consumer = new KafkaConsumer[String, QueueEvent](
    config.getConfig("waves.dex.events-queue.kafka.consumer.client").toProperties,
    new StringDeserializer,
    eventDeserializer
  )

  try {
    val topicPartition  = new TopicPartition(topic, 0)
    val topicPartitions = java.util.Collections.singletonList(topicPartition)
    consumer.assign(topicPartitions)

    {
      val r = consumer.partitionsFor(topic, requestTimeout)
      println(s"Partitions:\n${r.asScala.mkString("\n")}")
    }

    {
      val r = consumer.endOffsets(topicPartitions, requestTimeout)
      println(s"End offsets for $topicPartition: ${r.asScala.mkString(", ")}")
    }

    consumer.seek(topicPartition, from)

    val pollDuriation = java.time.Duration.ofNanos(1.seconds.toNanos)
    val lastOffset    = from + max
    var continue      = true
    while (continue) {
      println(s"Reading from Kafka")

      val xs = consumer.poll(pollDuriation).asScala.toVector
      xs.foreach { msg =>
        println(QueueEventWithMeta(msg.offset(), msg.timestamp(), msg.value()))
      }

      xs.lastOption.foreach { x =>
        if (x.offset() == lastOffset) continue = false
      }
    }
  } finally {
    consumer.close()
  }
}
