package com.wavesplatform.it

import java.net.InetSocketAddress
import java.util.concurrent.Executors

import cats.arrow.FunctionK
import cats.tagless._

import cats.tagless.implicits._
import cats.{Id, ~>}
import cats.instances.try_._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.{DexApi, LoggingSttpBackend, NodeApi, TracedDexApi}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, DockerContainer, WavesNodeContainer}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.util.Try

object NewMatcherSuiteBase {
  implicit val unwrap : Try ~> cats.Id = new FunctionK[Try, cats.Id] {
    def apply[A](fa: Try[A]): cats.Id[A] = fa.get
  }
}

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with ScorexLogging {

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newCachedThreadPool(new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build()))

  protected implicit val tryHttpBackend = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected val dockerClient: Coeval[docker.Docker] = Coeval.evalOnce(docker.Docker(getClass))

  // Waves miner node

  protected def wavesNode1Config: Config = DexTestConfig.containerConfig("waves-1")
  protected val wavesNode1Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-1", wavesNode1Config.resolve())
  }
  protected def wavesNode1Api: NodeApi[Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(wavesNode1Container(), wavesNode1Config.getInt("waves.rest-api.port"))
    implicit val t = NodeApi[Try]("integration-test-rest-api", apiAddress)
    NodeApi[Id]("integration-test-rest-api", apiAddress))
  }
  protected def wavesNode1NetworkApiAddress: InetSocketAddress =
    dockerClient().getInternalSocketAddress(wavesNode1Container(), wavesNode1Config.getInt("waves.network.port"))

  // Dex server

  protected def dex1Config: Config                 = DexTestConfig.containerConfig("dex-1")
  protected def dex1NodeContainer: DockerContainer = wavesNode1Container()
  protected val dex1Container: Coeval[DexContainer] = Coeval.evalOnce {
    val grpcAddr            = dockerClient().getInternalSocketAddress(dex1NodeContainer, dex1NodeContainer.config.getInt("waves.dex.grpc.integration.port"))
    val wavesNodeGrpcConfig = ConfigFactory.parseString(s"""waves.dex.waves-node-grpc {
                                                           |  host = ${grpcAddr.getAddress.getHostAddress}
                                                           |  port = ${grpcAddr.getPort}
                                                           |}""".stripMargin)
    dockerClient().createDex("dex-1", wavesNodeGrpcConfig.withFallback(dex1Config).resolve())
  }
  protected def dex1Api: TracedDexApi[Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(dex1Container(), dex1Config.getInt("waves.dex.rest-api.port"))
    TracedDexApi.wrap(DexApi.unWrapped(DexApi[Try](apiAddress)))
  }

  protected def allContainers: List[DockerContainer] = List(wavesNode1Container, dex1Container).map(x => x())

  override protected def beforeAll(): Unit = {
    log.debug(s"Doing beforeAll")
    super.beforeAll()
    allContainers.foreach(dockerClient().start)
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Doing afterAll")
    dockerClient().close()
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
