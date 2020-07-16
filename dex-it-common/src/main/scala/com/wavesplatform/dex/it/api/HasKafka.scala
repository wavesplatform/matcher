package com.wavesplatform.dex.it.api

import java.util.concurrent.ThreadLocalRandom

import com.dimafeng.testcontainers.KafkaContainer
import com.github.dockerjava.api.model.{ContainerNetwork, NetworkSettings}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.test.InformativeTestStart

import scala.jdk.CollectionConverters._

trait HasKafka { self: BaseContainersKit with InformativeTestStart =>

  protected val kafkaContainerName = s"$networkName-kafka"

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
        cmd withIpv4Address kafkaIp
      }
    }

  protected def disconnectKafkaFromNetwork(): Unit = {
    writeGlobalLog("--- Disconnecting Kafka from the network ---")

    kafka.dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(kafka.containerId)
      .withNetworkId(network.getId)
      .exec()

    waitForNetworkSettings(!_.getNetworks.containsKey(network.getId))
  }

  protected def connectKafkaToNetwork(): Unit = {
    writeGlobalLog("--- Connecting Kafka to the network ---")

    kafka.dockerClient
      .connectToNetworkCmd()
      .withContainerId(kafka.containerId)
      .withNetworkId(network.getId)
      .withContainerNetwork(
        new ContainerNetwork()
          .withIpamConfig(new ContainerNetwork.Ipam().withIpv4Address(kafkaIp))
          .withAliases(kafka.networkAliases.asJava))
      .exec()

    waitForNetworkSettings(_.getNetworks.containsKey(network.getId))
  }

  private def waitForNetworkSettings(pred: NetworkSettings => Boolean): Unit =
    Iterator
      .continually {
        Thread.sleep(1000)
        kafka.dockerClient.inspectContainerCmd(kafka.containerId).exec().getNetworkSettings
      }
      .zipWithIndex
      .find { case (ns, attempt) => pred(ns) || attempt == 10 }
      .fold(log.warn(s"Can't wait on ${kafka.containerId}"))(_ => ())
}
