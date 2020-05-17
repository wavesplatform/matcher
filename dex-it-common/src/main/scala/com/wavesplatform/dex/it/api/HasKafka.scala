package com.wavesplatform.dex.it.api

import java.util.concurrent.ThreadLocalRandom

import com.dimafeng.testcontainers.KafkaContainer
import com.typesafe.config.{Config, ConfigFactory}

trait HasKafka { self: BaseContainersKit =>

  private val kafkaContainerName = s"$networkName-kafka"

  protected val kafkaIp = getIp(12)

  protected def dexKafkaConfig(topic: String = ThreadLocalRandom.current.nextInt(0, Int.MaxValue).toString): Config = ConfigFactory.parseString(
    s"""waves.dex.events-queue {
       |  type = kafka
       |  kafka {
       |    servers = "$kafkaIp:9092"
       |    topic = "$topic"
       |  }
       |}""".stripMargin
  )

  protected val kafka: KafkaContainer =
    KafkaContainer().configure { k =>
      k.withNetwork(network)
      k.withNetworkAliases(kafkaContainerName)
      k.withCreateContainerCmdModifier { cmd =>
        cmd withName kafkaContainerName
        cmd withIpv4Address getIp(12)
      }
    }

  kafka.start()
}
