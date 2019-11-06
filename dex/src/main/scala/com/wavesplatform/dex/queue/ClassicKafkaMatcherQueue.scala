package com.wavesplatform.dex.queue

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.{Timer, TimerTask}

import akka.kafka._
import akka.kafka.scaladsl.Consumer
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.SourceQueueWithComplete
import com.wavesplatform.dex.queue.ClassicKafkaMatcherQueue.{KafkaProducer, eventDeserializer}
import com.wavesplatform.dex.queue.KafkaMatcherQueue.Settings
import com.wavesplatform.dex.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.dex.queue.QueueEventWithMeta.Offset
import com.wavesplatform.utils.ScorexLogging
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{Callback, ProducerRecord, RecordMetadata}
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization._

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContextExecutor, Future, Promise}

class ClassicKafkaMatcherQueue(settings: Settings)(implicit mat: ActorMaterializer) extends MatcherQueue with ScorexLogging {
  private implicit val dispatcher: ExecutionContextExecutor = mat.system.dispatcher

  @volatile private var lastUnreadOffset: QueueEventWithMeta.Offset = -1L

  private val timer = new Timer("local-dex-queue", true)

  private val duringShutdown = new AtomicBoolean(false)

  private val producer: Producer = {
    val r = if (settings.producer.enable) new KafkaProducer(settings, duringShutdown.get()) else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  private val consumerControl = new AtomicReference[Consumer.Control](Consumer.NoopControl)
  private val consumerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.consumer")
    ConsumerSettings(config, new ByteArrayDeserializer, eventDeserializer).withClientId("consumer")
  }

  @volatile private var lastProcessedOffsetInternal = -1L

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: Seq[QueueEventWithMeta] => Future[Unit]): Unit = {
    val topicPartition = new TopicPartition(settings.topic, 0) // Only one partition
    val consumer       = new KafkaConsumer[String, QueueEvent](consumerSettings.getProperties, new StringDeserializer, eventDeserializer)
    consumer.assign(java.util.Collections.singletonList(topicPartition))
    consumer.seek(topicPartition, fromOffset)

    def loop(): Unit = {
      val timerTask = new TimerTask {
        private val duration = java.time.Duration.ofNanos(consumerSettings.pollInterval.toNanos)

        override def run(): Unit = {
          val records = consumer.poll(duration)
          val size    = records.count()
          val xs = records.asScala.map { record =>
            QueueEventWithMeta(record.offset(), record.timestamp(), record.value())
          }.toSeq
          process(xs).andThen {
            case _ =>
              lastProcessedOffsetInternal += size
              loop()
          }
        }
      }

      timer.schedule(timerTask, 0)
    }

    loop()
  }

  override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = producer.storeEvent(event)

  override def lastProcessedOffset: Offset = lastProcessedOffsetInternal

  override def lastEventOffset: Future[QueueEventWithMeta.Offset] = {
    val metadataConsumer = new KafkaConsumer[String, QueueEvent](consumerSettings.getProperties, new StringDeserializer, eventDeserializer)
    val topicPartition   = new TopicPartition(settings.topic, 0)
    metadataConsumer.assign(java.util.Collections.singletonList(topicPartition))
    metadataConsumer.seekToEnd(java.util.Collections.singletonList(topicPartition))
    Future.successful(metadataConsumer.position(topicPartition) - 1)
  }

  override def close(timeout: FiniteDuration): Unit = {
    producer.close(timeout)
    duringShutdown.set(true)
    timer.cancel()
    val stoppingConsumer = consumerControl.get().shutdown()
    Await.result(stoppingConsumer, timeout)
  }

}

object ClassicKafkaMatcherQueue {
  val eventDeserializer: Deserializer[QueueEvent] = new Deserializer[QueueEvent] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def deserialize(topic: String, data: Array[Byte]): QueueEvent          = QueueEvent.fromBytes(data)
    override def close(): Unit                                                      = {}
  }

  val eventSerializer: Serializer[QueueEvent] = new Serializer[QueueEvent] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def serialize(topic: String, data: QueueEvent): Array[Byte]            = QueueEvent.toBytes(data)
    override def close(): Unit                                                      = {}
  }

  private class KafkaProducer(settings: Settings, duringShutdown: => Boolean)(implicit mat: ActorMaterializer) extends Producer {
    private type InternalProducer = SourceQueueWithComplete[(QueueEvent, Promise[QueueEventWithMeta])]

    private implicit val dispatcher: ExecutionContextExecutor = mat.system.dispatcher

    private val producerSettings = {
      val config = mat.system.settings.config.getConfig("akka.kafka.producer")
      akka.kafka.ProducerSettings(config, new ByteArraySerializer, eventSerializer)
    }

    private val producer =
      new org.apache.kafka.clients.producer.KafkaProducer[String, QueueEvent](producerSettings.getProperties, new StringSerializer, eventSerializer)

    override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = {
      val p = Promise[QueueEventWithMeta]()

      producer.send(
        new ProducerRecord[String, QueueEvent](settings.topic, event),
        new Callback {
          override def onCompletion(metadata: RecordMetadata, exception: Exception): Unit = {
            if (exception == null)
              p.success(
                QueueEventWithMeta(
                  offset = metadata.offset(),
                  timestamp = metadata.timestamp(),
                  event = event
                ))
            else p.failure(exception)
          }
        }
      )

      p.future.map(Some(_))
    }

    override def close(timeout: FiniteDuration): Unit = {
      producer.close(timeout.length, timeout.unit)
    }
  }
}
