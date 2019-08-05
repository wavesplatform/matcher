package com.wavesplatform.it

import java.net.InetSocketAddress

import cats.Id
import cats.instances.try_._
import com.softwaremill.sttp._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.{DexApi, NodeApi}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, WavesNodeContainer}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.ExecutionContext
import scala.util.Try

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with ScorexLogging {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  private implicit val tryHttpBackend: SttpBackend[Try, Nothing] = TryHttpURLConnectionBackend()

  protected val dockerClient: Coeval[docker.Docker] = Coeval.evalOnce(docker.Docker(getClass))

  protected def getWavesNodeApiAddress: InetSocketAddress =
    dockerClient().getInetSocketAddress(wavesNodeContainer(), wavesNodeConfig.getInt("waves.rest-api.port"))
  protected def wavesNodeApi: NodeApi[Id] = NodeApi.unWrapped(NodeApi[Try](getWavesNodeApiAddress))
  protected def wavesNodeConfig: Config   = DexTestConfig.containerConfig("waves-5")
  protected val wavesNodeContainer: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-5", wavesNodeConfig.resolve())
  }

  protected def getDexApiAddress: InetSocketAddress = dockerClient().getInetSocketAddress(dexContainer(), dexConfig.getInt("waves.dex.rest-api.port"))
  protected def dexApi: DexApi[Id]                  = DexApi.unWrapped(DexApi[Try](getDexApiAddress))
  protected def dexConfig: Config                   = DexTestConfig.containerConfig("dex-1")
  protected val dexContainer: Coeval[DexContainer] = Coeval.evalOnce {
    val grpcAddr = dockerClient().getInetSocketAddress(wavesNodeContainer(), wavesNodeConfig.getInt("waves.dex.grpc.integration.port"))
    println(grpcAddr)
    val wavesNodeGrpcConfig = ConfigFactory.parseString(s"""
      |waves.dex.waves-node-grpc {
      |  host = ${grpcAddr.getAddress.getHostAddress}
      |  port = ${grpcAddr.getPort}
      |}""".stripMargin)
    dockerClient().createDex("dex-1", wavesNodeGrpcConfig.withFallback(dexConfig).resolve())
  }

  override protected def beforeAll(): Unit = {
    log.debug(s"Doing beforeAll")
    super.beforeAll()

    List(wavesNodeContainer, dexContainer).foreach { x =>
      dockerClient().start(x())
    }
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Doing afterAll")
    List(wavesNodeContainer, dexContainer).foreach(x => dockerClient().stop(x()))
    super.afterAll()
  }
}
