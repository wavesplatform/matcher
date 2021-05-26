package com.wavesplatform.dex.it.docker

import cats.implicits._
import com.github.dockerjava.api.command.CreateContainerCmd
import com.github.dockerjava.api.model.Ports.Binding
import com.github.dockerjava.api.model.{ExposedPort, PortBinding}
import com.wavesplatform.dex.domain.utils.ScorexLogging

import java.net.ServerSocket
import java.util
import scala.jdk.CollectionConverters._
import scala.util.{Random, Using}

object PortBindingKeeper extends ScorexLogging {

  case class AvailablePortsNotFound() extends Throwable

  private val stringRange = Option(System.getenv("TEST_PORT_RANGE"))
    .getOrElse(throw new RuntimeException("Please specify the TEST_PORT_RANGE environment variable"))

  private val portsRange = parsePortRange()
  private var currentPosition = portsRange.start + Random.nextInt(portsRange.size)

  def getBindings(exposedPorts: Seq[Int]): util.List[PortBinding] = this.synchronized {
    log.debug(s"Getting ports for $exposedPorts")
    findFreePorts(exposedPorts.size).map { result =>
      exposedPorts.map(new ExposedPort(_)).zip(result.map(Binding.bindPort)).map { ports =>
        new PortBinding(ports._2, ports._1)
      }
    } match {
      case Right(v) =>
        log.debug("Successfully generated ports")
        v.asJava
      case Left(ex) => throw ex
    }
  }

  def getBindings(cmd: CreateContainerCmd, exposedPorts: Seq[Int]): util.List[PortBinding] = {
    val cmdExposedPorts = cmd.getExposedPorts.toList.map(_.getPort)
    PortBindingKeeper.getBindings(cmdExposedPorts ++ exposedPorts)
  }

  def getBindings(exposedPort: Int): util.List[PortBinding] =
    getBindings(Seq(exposedPort))

  private def findFreePorts(portNumbers: Int): Either[AvailablePortsNotFound, Seq[Int]] =
    (0 to portNumbers).foldLeft(Seq.empty[Int].asRight[AvailablePortsNotFound]) { (acc, _) =>
      acc.flatMap { accValue =>
        findFreePort().map(_ +: accValue)
      }
    }

  private def findFreePort(): Either[AvailablePortsNotFound, Int] = {
    val result = (currentPosition to portsRange.end).find { i =>
      Using(new ServerSocket(i))(_.getLocalPort).isSuccess
    }.orElse {
      (portsRange.start to currentPosition).find { i =>
        Using(new ServerSocket(i))(_.getLocalPort).isSuccess
      }
    }
    result match {
      case Some(value) =>
        currentPosition = (value - 1).min(portsRange.end)
        log.debug(s"Got port: $value and current position is $currentPosition")
        Right(value)

      case None => Left(AvailablePortsNotFound())
    }
  }

  private def parsePortRange(): Range.Inclusive = {
    val limits = stringRange.split('-')
    if (limits.length != 2) throw new IllegalArgumentException(s"Illegal port range for tests! $stringRange")
    val first = limits.head.toInt
    val second = limits.last.toInt
    if (first >= second)
      throw new IllegalArgumentException(s"Illegal port range for tests! First boundary $first is bigger or equals second $second!")
    first to second
  }

}
