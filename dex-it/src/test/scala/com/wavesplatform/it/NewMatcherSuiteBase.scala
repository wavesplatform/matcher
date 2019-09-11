package com.wavesplatform.it

import java.net.InetSocketAddress
import java.util.concurrent.{Executors, ThreadLocalRandom}

import cats.Id
import cats.instances.future._
import cats.instances.try_._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.it.api.{CanExtract, DexApi, DexOps, HasWaitReady, LoggingSttpBackend, NodeApi}
import com.wavesplatform.it.blockchain.MkEntities
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.dex.DoubleOps
import com.wavesplatform.it.docker.{DexContainer, DockerContainer, DockerExtensions, WavesNodeContainer}
import com.wavesplatform.it.test.{ApiExtensions, ItMatchers}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest._
import org.scalatest.concurrent.Eventually

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

abstract class NewMatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with Eventually
    with MkEntities
    with ApiExtensions
    with ItMatchers
    with DockerExtensions
    with DoubleOps
    with ScorexLogging {

  protected implicit def toDexExplicitGetOps[F[_]](self: DexApi[F])(implicit E: CanExtract[F]) = new DexOps.ExplicitGetOps[F](self)

  protected def suiteInitialWavesNodeConfig: Config = ConfigFactory.empty()
  protected def suiteInitialDexConfig: Config       = ConfigFactory.empty()

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'Y'.toByte
  }

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor {
    Executors.newCachedThreadPool(new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build)
  }

  protected implicit val futureHttpBackend = new LoggingSttpBackend[Future, Nothing](AsyncHttpClientFutureBackend())
  protected implicit val tryHttpBackend    = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected val internalDockerClient: Coeval[docker.Docker] = Coeval.evalOnce { docker.Docker(getClass) }
  override protected def dockerClient: docker.Docker        = internalDockerClient()

  // Waves miner node
  protected val wavesNodeRunConfig: Coeval[Config] = Coeval.evalOnce(DexTestConfig.genesisConfig)

  protected val wavesNode1Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient.createWavesNode("waves-1", wavesNodeRunConfig(), suiteInitialWavesNodeConfig)
  }

  protected def wavesNode1Api: NodeApi[cats.Id] = {
    def apiAddress = dockerClient.getExternalSocketAddress(wavesNode1Container(), wavesNode1Container().restApiPort)
    fp.sync(NodeApi[Try]("integration-test-rest-api", apiAddress))
  }

  protected def wavesNode1NetworkApiAddress: InetSocketAddress =
    dockerClient.getInternalSocketAddress(wavesNode1Container(), wavesNode1Container().networkApiPort)

  // Dex server
  protected val dexRunConfig: Coeval[Config] = Coeval.evalOnce {
    dexQueueConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
      .withFallback(dexWavesGrpcConfig(wavesNode1Container()))
      .withFallback(DexTestConfig.updatedMatcherConfig)
  }

  protected val dex1Container: Coeval[DexContainer] = Coeval.evalOnce {
    dockerClient.createDex("dex-1", dexRunConfig(), suiteInitialDexConfig)
  }

  private def dex1ApiAddress                 = dockerClient.getExternalSocketAddress(dex1Container(), dex1Container().restApiPort)
  protected def dex1AsyncApi: DexApi[Future] = DexApi[Future]("integration-test-rest-api", dex1ApiAddress)
  protected def dex1Api: DexApi[Id]          = fp.sync(DexApi[Try]("integration-test-rest-api", dex1ApiAddress))

  protected def allContainers: List[DockerContainer] = List(wavesNode1Container, dex1Container).map(x => x())
  protected def allApis: List[HasWaitReady[cats.Id]] = List(wavesNode1Api, dex1Api)

  override protected def beforeAll(): Unit = {
    log.debug(s"Doing beforeAll")
    super.beforeAll()

    val (waves, dex) = allContainers.partition {
      case _: WavesNodeContainer => true
      case _                     => false
    }

    val (wavesApi, dexApi) = allApis.partition {
      case _: NodeApi[Id] => true
      case _              => false
    }

    waves.foreach(dockerClient.start)
    wavesApi.foreach(_.waitReady)

    dex.foreach(dockerClient.start)
    dexApi.foreach(_.waitReady)
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Doing afterAll")
    dockerClient.close()
    futureHttpBackend.close()
    tryHttpBackend.close()
    super.afterAll()
  }

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

  protected def dexQueueConfig(queueId: Int): Config = Option(System.getenv("KAFKA_SERVER")).fold(ConfigFactory.empty()) { kafkaServer =>
    ConfigFactory.parseString(s"""waves.dex.events-queue {
                                 |  type = kafka
                                 |  kafka {
                                 |    servers = "$kafkaServer"
                                 |    topic = "dex-$queueId"
                                 |  }
                                 |}""".stripMargin)
  }

  protected def dexWavesGrpcConfig(target: WavesNodeContainer): Config = {
    val grpcAddr = dockerClient.getInternalSocketAddress(target, target.grpcApiPort)
    ConfigFactory
      .parseString(s"""waves.dex {
                      |  waves-node-grpc {
                      |    host = ${grpcAddr.getAddress.getHostAddress}
                      |    port = ${grpcAddr.getPort}
                      |  }
                      |}""".stripMargin)
  }
}
