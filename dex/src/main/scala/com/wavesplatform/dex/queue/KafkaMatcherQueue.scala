package com.wavesplatform.dex.queue

import java.util
import java.util.Properties
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executors, TimeoutException}

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.queue.KafkaMatcherQueue.{validatedCommandDeserializer, KafkaProducer, Settings}
import com.wavesplatform.dex.queue.MatcherQueue.{IgnoreProducer, Producer}
import com.wavesplatform.dex.settings.toConfigOps
import monix.eval.Task
import monix.execution.{Cancelable, ExecutionModel, Scheduler}
import monix.reactive.Observable
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{Callback, ProducerRecord, RecordMetadata}
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.errors.{WakeupException, TimeoutException => KafkaTimeoutException}
import org.apache.kafka.common.serialization._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{blocking, ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters._
import scala.jdk.DurationConverters.ScalaDurationOps

class KafkaMatcherQueue(settings: Settings) extends MatcherQueue with ScorexLogging {

  private val producerThreadPool =
    Executors.newFixedThreadPool(3, new ThreadFactoryBuilder().setDaemon(false).setNameFormat("queue-kafka-producer-%d").build())

  private val producerExecutionContext = ExecutionContext.fromExecutor(producerThreadPool)

  private val consumerThreadPool =
    Executors.newSingleThreadExecutor(new ThreadFactoryBuilder().setDaemon(false).setNameFormat("queue-kafka-consumer-%d").build())

  private val consumerExecutionContext = ExecutionContext.fromExecutor(consumerThreadPool)

  private val duringShutdown = new AtomicBoolean(false)

  private val topicPartition = new TopicPartition(settings.topic, 0) // Only one partition
  private val topicPartitions: util.List[TopicPartition] = java.util.Collections.singletonList(topicPartition)

  private val consumerConfig = settings.consumer.client

  private val consumer =
    new KafkaConsumer[String, ValidatedCommand](consumerConfig.toProperties, new StringDeserializer, validatedCommandDeserializer)

  consumer.assign(topicPartitions)
  private val pollDuration = java.time.Duration.ofMillis(settings.consumer.fetchMaxDuration.toMillis)

  @volatile private var consumerTask: Cancelable = Cancelable.empty

  private val producer: Producer = {
    val r =
      if (settings.producer.enable) new KafkaProducer(settings.topic, settings.producer.client.toProperties)(producerExecutionContext)
      else IgnoreProducer
    log.info(s"Choosing ${r.getClass.getName} producer")
    r
  }

  private val pollTask: Task[IndexedSeq[ValidatedCommandWithMeta]] = Task {
    try if (duringShutdown.get()) IndexedSeq.empty
    else {
      val records = consumer.poll(pollDuration)
      records.asScala.map { record =>
        ValidatedCommandWithMeta(record.offset(), record.timestamp(), record.value())
      }.toIndexedSeq
    } catch {
      case e: WakeupException => if (duringShutdown.get()) IndexedSeq.empty else throw e
      case e: Throwable =>
        log.error(s"Can't consume", e)
        IndexedSeq.empty
    }
  }

  override def startConsume(fromOffset: ValidatedCommandWithMeta.Offset, process: Seq[ValidatedCommandWithMeta] => Future[Unit]): Unit = {
    val scheduler = Scheduler(consumerExecutionContext, executionModel = ExecutionModel.AlwaysAsyncExecution)
    consumerTask.cancel()
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
            Task(log.error("Consumer fails", e))
          }
      }
      .subscribe()(scheduler)
  }

  override def store(command: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]] = producer.store(command)

  override def firstOffset: Future[ValidatedCommandWithMeta.Offset] =
    safeOffsetRequest(consumer.beginningOffsets(topicPartitions).get(topicPartition) - 1L)

  override def lastOffset: Future[ValidatedCommandWithMeta.Offset] =
    safeOffsetRequest(consumer.endOffsets(topicPartitions).get(topicPartition) - 1L)

  private def safeOffsetRequest(f: => ValidatedCommandWithMeta.Offset): Future[ValidatedCommandWithMeta.Offset] = {
    implicit val context = consumerExecutionContext
    Future(consumer.listTopics())
      .flatMap { topics =>
        val partitions = topics.getOrDefault(settings.topic, java.util.Collections.emptyList()).asScala
        if (partitions.size > 1) Future.failed(new IllegalStateException(s"DEX can work only with one partition, given: $partitions"))
        else if (partitions.headOption.isEmpty) Future.successful(-1L)
        else Future(f)
      }
      .recoverWith {
        case e: KafkaTimeoutException =>
          log.error("Can't receive information in time", e)
          throw new TimeoutException("Can't receive information in time")
      }
  }

  override def close(timeout: FiniteDuration): Future[Unit] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    duringShutdown.set(true)

    val stopProducer = Future(blocking(producer.close(timeout)))(producerExecutionContext).andThen(_ => producerThreadPool.shutdown())
    val stopConsumer = Future(blocking {
      consumer.close(timeout.toJava)
      consumerTask.cancel()
    })(consumerExecutionContext).andThen(_ => consumerThreadPool.shutdown())

    stopProducer zip stopConsumer map (_ => ())
  }

}

object KafkaMatcherQueue {
  case class Settings(topic: String, consumer: ConsumerSettings, producer: ProducerSettings)
  case class ConsumerSettings(fetchMaxDuration: FiniteDuration, maxBufferSize: Int, client: Config)
  case class ProducerSettings(enable: Boolean, client: Config)

  val validatedCommandDeserializer: Deserializer[ValidatedCommand] = new Deserializer[ValidatedCommand] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def deserialize(topic: String, data: Array[Byte]): ValidatedCommand = ValidatedCommand.fromBytes(data)
    override def close(): Unit = {}
  }

  val validatedCommandSerializer: Serializer[ValidatedCommand] = new Serializer[ValidatedCommand] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def serialize(topic: String, data: ValidatedCommand): Array[Byte] = ValidatedCommand.toBytes(data)
    override def close(): Unit = {}
  }

  private class KafkaProducer(topic: String, producerSettings: Properties)(implicit ec: ExecutionContext) extends Producer with ScorexLogging {

    private val producer =
      new org.apache.kafka.clients.producer.KafkaProducer[String, ValidatedCommand](
        producerSettings,
        new StringSerializer,
        validatedCommandSerializer
      )

    override def store(command: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]] = {
      log.trace(s"Storing $command")
      val p = Promise[ValidatedCommandWithMeta]()
      try producer.send(
        new ProducerRecord[String, ValidatedCommand](topic, command),
        new Callback {
          override def onCompletion(metadata: RecordMetadata, exception: Exception): Unit =
            if (exception == null) {
              log.debug(s"Stored $command, offset=${metadata.offset()}, timestamp=${metadata.timestamp()}")
              p.success(
                ValidatedCommandWithMeta(
                  offset = metadata.offset(),
                  timestamp = metadata.timestamp(),
                  command = command
                )
              )
            } else {
              log.error(s"During storing $command", exception)
              p.failure(exception match {
                case _: KafkaTimeoutException => new TimeoutException(s"Can't store message $command")
                case _ => exception
              })
            }
        }
      )
      catch {
        case e: Throwable =>
          log.error(s"Can't store message $command", e)
          p.failure(e)
      }

      p.future.map(Some(_))
    }

    override def close(timeout: FiniteDuration): Unit =
      producer.close(java.time.Duration.ofNanos(timeout.toNanos))

  }

}
