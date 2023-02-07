package com.wavesplatform.dex.it.api

import java.util.concurrent.ThreadLocalRandom
import com.github.dockerjava.api.model.{ContainerNetwork, ExposedPort, NetworkSettings, Ports}
import com.redis.testcontainers.RedisContainer
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.docker.PortBindingKeeper
import com.wavesplatform.dex.it.test.InformativeTestStart
import org.testcontainers.utility.DockerImageName

import java.net.InetSocketAddress
import scala.util.chaining._
import scala.jdk.CollectionConverters._

trait HasRedis { self: BaseContainersKit with InformativeTestStart =>

  protected val redisContainerName = s"$networkName-redis"

  protected val redisImage = "redis:latest"

  protected val redisIp = getIp(1130)
  protected val redisPort = 6379

  protected def redisStreamName = "dex-internal-events"

  protected def dexRedisConfig(streamName: String = ThreadLocalRandom.current.nextInt(0, Int.MaxValue).toString): Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  redis-internal-client-handler-actor {
         |    enabled = true
         |    stream-name = "$streamName"
         |  }
         |
         |  redis {
         |    address = "redis://$redisIp:$redisPort"
         |  }
         |}""".stripMargin
    )

  protected val redis: RedisContainer = new RedisContainer(DockerImageName.parse(redisImage)).pipe { container =>
    container.withNetwork(network)
    container.withNetworkAliases(redisContainerName)
    container.withCreateContainerCmdModifier { cmd =>
      cmd.withName(redisContainerName)
      cmd.withIpv4Address(redisIp)

      cmd.getHostConfig
        .withPortBindings(PortBindingKeeper.getBindings(cmd, Seq(redisPort)))
    }

  }

  protected lazy val externalRedisAddress: InetSocketAddress = getAddress(redisPort)

  protected def disconnectRedisFromNetwork(): Unit = {
    writeGlobalLog("--- Disconnecting Redis from the network ---")

    redis.getDockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(redis.getContainerId)
      .withNetworkId(network.getId)
      .exec()

    waitForNetworkSettings(!_.getNetworks.containsKey(network.getId))
  }

  protected def connectRedisToNetwork(): Unit = {
    writeGlobalLog("--- Connecting Redis to the network ---")

    redis.getDockerClient
      .connectToNetworkCmd()
      .withContainerId(redis.getContainerId)
      .withNetworkId(network.getId)
      .withContainerNetwork(
        new ContainerNetwork()
          .withIpamConfig(new ContainerNetwork.Ipam().withIpv4Address(redisIp))
          .withAliases(redis.getNetworkAliases)
      )
      .exec()

    waitForNetworkSettings(_.getNetworks.containsKey(network.getId))
  }

  private def waitForNetworkSettings(pred: NetworkSettings => Boolean): Unit =
    Iterator
      .continually {
        Thread.sleep(1000)
        redis.getDockerClient.inspectContainerCmd(redis.getContainerId).exec().getNetworkSettings
      }
      .zipWithIndex
      .find { case (ns, attempt) => pred(ns) || attempt == 10 }
      .fold(log.warn(s"Can't wait on ${redis.getContainerId}"))(_ => ())

  protected def getAddress(port: Int) = {
    val maybeBindings: Option[Array[Ports.Binding]] =
      redis.getDockerClient
        .inspectContainerCmd(redis.getContainerId)
        .exec()
        .getNetworkSettings
        .getPorts
        .getBindings
        .asScala
        .get(new ExposedPort(port))
        .flatMap(Option(_))

    val externalPort =
      maybeBindings
        .flatMap(_.headOption)
        .map(_.getHostPortSpec.toInt)
        .getOrElse(throw new IllegalStateException(s"There is no mapping '$port/tcp' for '${redis.getContainerName}'"))

    new InetSocketAddress(redis.getHost, externalPort)
  }

}
