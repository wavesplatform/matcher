package com.wavesplatform.it

import java.net.InetSocketAddress
import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

import cats.Id
import cats.instances.try_._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.{DexApi, LoggingSttpBackend, NodeApi, TracedDexApi}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, DockerContainer, WavesNodeContainer}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest.{Args, BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers, Status}

import scala.concurrent.ExecutionContext
import scala.util.Try

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with ScorexLogging {

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newCachedThreadPool(new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build()))

  protected implicit val tryHttpBackend = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected val dockerClient: Coeval[docker.Docker] = Coeval.evalOnce(docker.Docker(getClass))

  protected def getWavesNodeRestApiAddress: InetSocketAddress =
    dockerClient().getExternalSocketAddress(wavesNodeContainer(), wavesNodeConfig.getInt("waves.rest-api.port"))
  protected def getWavesNodeNetworkApiAddress: InetSocketAddress =
    dockerClient().getInternalSocketAddress(wavesNodeContainer(), wavesNodeConfig.getInt("waves.network.port"))
  protected def wavesNodeApi: NodeApi[Id] = NodeApi.unWrapped(NodeApi[Try]("integration-test-rest-api", getWavesNodeRestApiAddress))
  protected def wavesNodeName = "waves-5"
  protected def wavesNodeConfig: Config   = DexTestConfig.containerConfig(wavesNodeName)
  protected val wavesNodeContainer: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode(wavesNodeName, wavesNodeConfig.resolve())
  }

  protected def getDexApiAddress: InetSocketAddress =
    dockerClient().getExternalSocketAddress(dexContainer(), dexConfig.getInt("waves.dex.rest-api.port"))
  protected def dexApi: TracedDexApi[Id] = TracedDexApi.wrap(DexApi.unWrapped(DexApi[Try](getDexApiAddress)))
  protected def dexConfig: Config        = DexTestConfig.containerConfig("dex-1")
  protected val dexContainer: Coeval[DexContainer] = Coeval.evalOnce {
    val grpcAddr            = dockerClient().getInternalSocketAddress(wavesNodeContainer(), wavesNodeConfig.getInt("waves.dex.grpc.integration.port"))
    val wavesNodeGrpcConfig = ConfigFactory.parseString(s"""
      |waves.dex.waves-node-grpc {
      |  host = ${grpcAddr.getAddress.getHostAddress}
      |  port = ${grpcAddr.getPort}
      |}""".stripMargin)
    dockerClient().createDex("dex-1", wavesNodeGrpcConfig.withFallback(dexConfig).resolve())
  }

  protected def allContainers: List[DockerContainer] = List(wavesNodeContainer, dexContainer).map(x => x())

  override protected def beforeAll(): Unit = {
    log.debug(s"Doing beforeAll")
    super.beforeAll()

    // todo - dexContainer depends on wavesNodeContainer
    List(wavesNodeContainer, dexContainer).foreach { x =>
      dockerClient().start(x())
      Thread.sleep(5000)
    }
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Doing afterAll")
    allContainers.foreach(x => dockerClient().stop(x))
    super.afterAll()
  }

  override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    print(s"Test '$testName' ${if (r.succeeds()) "succeeded" else "failed"}")
    r
  }

  private def print(text: String): Unit = {
    val formatted = s"---------- $text ----------"
    log.debug(formatted)
    try allContainers.foreach(x => dockerClient().printDebugMessage(x, formatted))
    catch {
      case _: Throwable => ()
    }
  }

}
