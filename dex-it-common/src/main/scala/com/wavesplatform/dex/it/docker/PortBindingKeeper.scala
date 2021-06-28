package com.wavesplatform.dex.it.docker

import com.github.dockerjava.api.command.CreateContainerCmd
import com.github.dockerjava.api.model.Ports.Binding
import com.github.dockerjava.api.model.{ExposedPort, PortBinding}
import com.wavesplatform.dex.domain.utils.ScorexLogging

import java.net.ServerSocket
import java.util
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._
import scala.util.Using

object PortBindingKeeper extends ScorexLogging {

  case class AvailablePortsNotFound() extends Throwable

  private val DEFAULT_TEST_PORT_RANGE = 10000 to 10050

  private val portsRange = Option(System.getenv("TEST_PORT_RANGE"))
    .map(parsePortRange)
    .getOrElse(DEFAULT_TEST_PORT_RANGE)

  private var currentPosition = portsRange.start

  def getBindings(exposedPorts: Seq[Int]): util.List[PortBinding] = synchronized {
    log.debug(s"Getting ports for $exposedPorts")
    val result = findFreePorts(exposedPorts.size)
    log.debug(s"Successfully generated ports: $result")
    exposedPorts.map(new ExposedPort(_)).zip(result.map(Binding.bindPort)).map { ports =>
      new PortBinding(ports._2, ports._1)
    }.asJava
  }

  def getBindings(cmd: CreateContainerCmd, exposedPorts: Seq[Int]): util.List[PortBinding] = {
    val cmdExposedPorts = cmd.getExposedPorts.toList.map(_.getPort)
    getBindings(cmdExposedPorts ++ exposedPorts)
  }

  def getBindings(exposedPort: Int): util.List[PortBinding] =
    getBindings(Seq(exposedPort))

  private def findFreePorts(portNumbers: Int): List[Int] =
    (0 to portNumbers).foldLeft(List.empty[Int]) { (acc, _) =>
      findFreePort() +: acc
    }

  private def findFreePort(): Int =
    findFreePortIn(currentPosition to portsRange.end).fold(throw AvailablePortsNotFound()) { newPort =>
      currentPosition = (newPort + 1).min(portsRange.end)
      newPort
    }

  private def findFreePortIn(range: Range): Option[Int] =
    range.find { i =>
      Using(new ServerSocket(i))(_.getLocalPort).isSuccess
    }

  @nowarn
  private def parsePortRange(stringRange: String): Range.Inclusive = {
    val limits = stringRange.split('-').map(_.toInt)
    if (limits.length != 2) throw new IllegalArgumentException(s"Illegal port range for tests! $stringRange")
    val Array(first, second) = limits
    if (first >= second)
      throw new IllegalArgumentException(s"Illegal port range for tests! First boundary $first is bigger or equals second $second!")
    first to second
  }

}
