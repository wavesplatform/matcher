package com.wavesplatform.it.sync.kafka.issues

import com.wavesplatform.dex.Implicits.durationToScalatestTimeout
import com.wavesplatform.dex.app.QueueMessageDeserializationError
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.it.MatcherSuiteBase
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord, RecordMetadata}
import org.apache.kafka.common.serialization.StringSerializer

import java.util.Properties
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import scala.util.Using

class DeserializationIssuesTestSuite extends MatcherSuiteBase with HasKafka {

  private val topicName = s"test-${ThreadLocalRandom.current.nextInt(0, Int.MaxValue)}"
  override protected lazy val dexRunConfig = dexKafkaConfig(topicName)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    kafka.start()

    broadcastAndAwait(IssueUsdTx)
    createKafkaTopic(topicName, Some(kafka.bootstrapServers))
    dex1.start()
  }

  "Matcher should stop working when got unexpected kafka message" in {
    val kafkaProducerProps: Properties = {
      val props = new Properties()
      props.put("bootstrap.servers", kafka.bootstrapServers)
      val stringSerializerName = classOf[StringSerializer].getName
      props.put("key.serializer", stringSerializerName)
      props.put("value.serializer", stringSerializerName)
      props
    }
    val sendResult = Promise[Unit]()
    Using.resource(new KafkaProducer[String, String](kafkaProducerProps)) { producer =>
      producer.send(
        new ProducerRecord(topicName, "incorrect_key", "incorrect_value"),
        (_: RecordMetadata, exception: Exception) =>
          Option(exception) match {
            case Some(e) => sendResult.failure(e)
            case None => sendResult.success(())
          }
      )
      sendResult.future.futureValue(10.seconds)
    }
    eventually {
      dex1.getState().getExitCodeLong shouldBe QueueMessageDeserializationError.code
    }
  }
}
