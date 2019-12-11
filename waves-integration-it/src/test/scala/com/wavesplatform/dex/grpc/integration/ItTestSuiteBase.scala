package com.wavesplatform.dex.grpc.integration

import cats.Id
import cats.instances.try_._
import com.softwaremill.sttp.TryHttpURLConnectionBackend
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.{HasWaitReady, NodeApi}
import com.wavesplatform.dex.it.assets.DoubleOps
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.config.{GenesisConfig, PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.it.docker.{DockerContainer, WavesIntegrationItDocker, WavesNodeContainer}
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.dex.it.test.{HasWavesNode, WavesNodeApiExtensions}
import com.wavesplatform.dex.it.waves.{MkWavesEntities, WavesFeeConstants}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest._
import org.scalatest.concurrent.Eventually

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

trait ItTestSuiteBase
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with Eventually
    with HasWavesNode
    with MkWavesEntities
    with WavesFeeConstants
    with WavesNodeApiExtensions
    with PredefinedAssets
    with PredefinedAccounts
    with DoubleOps
    with DiffMatcherWithImplicits
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def suiteInitialWavesNodeConfig: Config = ConfigFactory.empty()

  protected implicit val tryHttpBackend = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected val internalDockerClient: Coeval[com.wavesplatform.dex.it.docker.Docker] = Coeval.evalOnce {
    com.wavesplatform.dex.it.docker.Docker(getClass)
  }

  protected def dockerClient: com.wavesplatform.dex.it.docker.Docker = internalDockerClient()

  protected def allContainers: List[DockerContainer] = List(wavesNode1Container) map { _.value }
  protected def allApis: List[HasWaitReady[Id]]      = List(wavesNode1Api)

  protected val wavesNodeRunConfig: Coeval[Config] = Coeval.evalOnce(GenesisConfig.config)

  protected val wavesNode1Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    createWavesNode("waves-1", wavesNodeRunConfig(), suiteInitialWavesNodeConfig)
  }

  protected val cachedWavesNode1ApiAddress = CachedData {
    dockerClient.getExternalSocketAddress(wavesNode1Container(), wavesNode1Container().restApiPort)
  }

  // MonadError can't be implemented for Id
  protected def wavesNode1Api: NodeApi[Id] = fp.sync(NodeApi[Try]("integration-test-rest-api", cachedWavesNode1ApiAddress.get()))

  protected val cachedWavesNode1GrpcApiTarget = CachedData {
    val x = dockerClient.getExternalSocketAddress(wavesNode1Container(), wavesNode1Container().grpcApiPort)
    s"${x.getHostName}:${x.getPort}"
  }
  protected def wavesNode1GrpcApiTarget: String = cachedWavesNode1GrpcApiTarget.get()

  protected val cachedWavesNode1NetworkApiAddress = CachedData {
    dockerClient.getInternalSocketAddress(wavesNode1Container(), wavesNode1Container().networkApiPort)
  }

  protected def createWavesNode(name: String, runConfig: Config, suiteInitialConfig: Config): WavesNodeContainer =
    WavesIntegrationItDocker.createContainer(dockerClient)(name, runConfig, suiteInitialConfig)

  override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    r.whenCompleted {
      case Success(r) => print(s"Test '$testName' ${if (r) "succeeded" else "failed"}")
      case Failure(e) => print(s"Test '$testName' failed with exception '${e.getClass.getSimpleName}'")
    }
    r
  }

  private def print(text: String): Unit = {
    val formatted = s"---------- $text ----------"
    log.debug(formatted)
    try allContainers.foreach(x => dockerClient.printDebugMessage(x, formatted))
    catch {
      case _: Throwable => ()
    }
  }

  override protected def beforeAll(): Unit = {
    log.debug(s"Doing beforeAll")
    super.beforeAll()

    allContainers.foreach(dockerClient.start)
    allApis.foreach(_.waitReady)
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Doing afterAll")
    dockerClient.close()
    tryHttpBackend.close()
    super.afterAll()
  }

  protected def invalidateCaches(): Unit = {
    cachedWavesNode1ApiAddress.invalidate()
    cachedWavesNode1GrpcApiTarget.invalidate()
    cachedWavesNode1NetworkApiAddress.invalidate()
  }
}
