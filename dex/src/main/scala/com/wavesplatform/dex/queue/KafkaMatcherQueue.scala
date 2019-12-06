package com.wavesplatform.dex.queue

import java.util
import java.util.Properties
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, TimeoutException}

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.queue.KafkaMatcherQueue.{KafkaProducer, Settings, eventDeserializer}
import com.wavesplatform.dex.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.dex.settings.toConfigOps
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{Cancelable, ExecutionModel, Scheduler}
import monix.reactive.Observable
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{Callback, ProducerRecord, RecordMetadata}
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.errors.{WakeupException, TimeoutException => KafkaTimeoutException}
import org.apache.kafka.common.serialization._

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}

class KafkaMatcherQueue(settings: Settings) extends MatcherQueue with ScorexLogging {

  private implicit val executionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(3, new ThreadFactoryBuilder().setDaemon(true).setNameFormat("kafka-%d").build()))

  private val duringShutdown = new AtomicBoolean(false)

  private val topicPartition                             = new TopicPartition(settings.topic, 0) // Only one partition
  private val topicPartitions: util.List[TopicPartition] = java.util.Collections.singletonList(topicPartition)

  private val consumerConfig = settings.consumer.client
  private val consumer       = new KafkaConsumer[String, QueueEvent](consumerConfig.toProperties, new StringDeserializer, eventDeserializer)
  consumer.assign(topicPartitions)
  private val pollDuration = java.time.Duration.ofMillis(settings.consumer.fetchMaxDuration.toMillis)

  @volatile private var consumerTask: Cancelable = Cancelable.empty

  // We need a separate metadata consumer because KafkaConsumer is not safe for multi-threaded access
  private val metadataConsumer = {
    val config = ConfigFactory
      .parseString(s"""
         |client.id = metadata-consumer
         |group.id = meta-${consumerConfig.getString("group.id")}""".stripMargin)
      .withFallback(consumerConfig)
    new KafkaConsumer[String, QueueEvent](config.toProperties, new StringDeserializer, eventDeserializer)
  }
  metadataConsumer.assign(topicPartitions)

  private val producer: Producer = {
    val r =
      if (settings.producer.enable) new KafkaProducer(settings.topic, settings.producer.client.toProperties)
      else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  private val pollTask: Task[IndexedSeq[QueueEventWithMeta]] = Task {
    try {
      val records = consumer.poll(pollDuration)
      records.asScala.map { record =>
        QueueEventWithMeta(record.offset(), record.timestamp(), record.value())
      }.toIndexedSeq
    } catch {
      case e: WakeupException => if (duringShutdown.get()) IndexedSeq.empty else throw e
      case e: Throwable =>
        log.error(s"Can't consume", e)
        IndexedSeq.empty
    }
  }

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: Seq[QueueEventWithMeta] => Future[Unit]): Unit = {
    val scheduler = Scheduler(executionContext, executionModel = ExecutionModel.AlwaysAsyncExecution)
    consumerTask = Observable
      .fromTask(Task {
        if (fromOffset > 0) consumer.seek(topicPartition, fromOffset)
        else consumer.seekToBeginning(topicPartitions)
      })
      .flatMap { _ =>
        Observable
          .repeatEvalF(pollTask)
          .flatMap(Observable.fromIterable)
          .bufferIntrospective(settings.consumer.maxBufferSize)
          .filter(_.nonEmpty)
          .mapEvalF(process)
          .doOnError { e =>
            Task { log.error("Consumer fails", e) }
          }
      }
      .subscribe()(scheduler)
  }

  override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = producer.storeEvent(event)

  override def lastEventOffset: Future[QueueEventWithMeta.Offset] =
    Future(metadataConsumer.endOffsets(topicPartitions).get(topicPartition) - 1).recoverWith {
      case e: KafkaTimeoutException => throw new TimeoutException("Can't receive information in time")
    }

  override def close(timeout: FiniteDuration): Unit = {
    duringShutdown.set(true)

    producer.close(timeout)

    val duration = java.time.Duration.ofNanos(timeout.toNanos)
    consumer.close(duration)
    metadataConsumer.close(duration)

    consumerTask.cancel()
  }
}

object KafkaMatcherQueue {
  case class Settings(topic: String, consumer: ConsumerSettings, producer: ProducerSettings)
  case class ConsumerSettings(fetchMaxDuration: FiniteDuration, maxBufferSize: Int, client: Config)
  case class ProducerSettings(enable: Boolean, client: Config)

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

  private class KafkaProducer(topic: String, producerSettings: Properties)(implicit ec: ExecutionContext) extends Producer with ScorexLogging {
    private val producer =
      new org.apache.kafka.clients.producer.KafkaProducer[String, QueueEvent](producerSettings, new StringSerializer, eventSerializer)

    override def storeEvent(event: QueueEvent): Future[Option[QueueEventWithMeta]] = {
      log.trace(s"Storing $event")

      val p = Promise[QueueEventWithMeta]()

      try {
        producer.send(
          new ProducerRecord[String, QueueEvent](topic, event),
          new Callback {
            override def onCompletion(metadata: RecordMetadata, exception: Exception): Unit = {
              if (exception == null) {
                log.debug(s"$event stored, offset=${metadata.offset()}, timestamp=${metadata.timestamp()}")
                p.success(
                  QueueEventWithMeta(
                    offset = metadata.offset(),
                    timestamp = metadata.timestamp(),
                    event = event
                  ))
              } else {
                log.error(s"During storing $event", exception)
                p.failure(exception)
              }
            }
          }
        )
      } catch {
        case e: Throwable => log.error(s"Can't store message $event", e)
      }

      p.future.map(Some(_))
    }

    override def close(timeout: FiniteDuration): Unit = {
      producer.close(java.time.Duration.ofNanos(timeout.toNanos))
    }
  }
}
