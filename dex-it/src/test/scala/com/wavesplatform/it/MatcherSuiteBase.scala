package com.wavesplatform.it

import java.net.InetSocketAddress
import java.util.concurrent.{Executors, ThreadLocalRandom}

import cats.instances.future.catsStdInstancesForFuture
import cats.instances.try_._
import cats.{Functor, Id}
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.sttp.TryHttpURLConnectionBackend
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.NodeApi
import com.wavesplatform.dex.it.assets.DoubleOps
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.config.{GenesisConfig, PredefinedAccounts, PredefinedAssets}
import com.wavesplatform.dex.it.docker._
import com.wavesplatform.dex.it.fp
import com.wavesplatform.dex.it.fp.CanExtract
import com.wavesplatform.dex.it.sttp.LoggingSttpBackend
import com.wavesplatform.dex.it.test.HasWavesNode
import com.wavesplatform.dex.it.waves.{MkWavesEntities, WavesFeeConstants}
import com.wavesplatform.it.api.{DexApi, DexApiOps}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, DexItDocker}
import com.wavesplatform.it.test.{ApiExtensions, ItMatchers}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.asynchttpclient.DefaultAsyncHttpClientConfig
import org.scalatest._
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Eventually
    with HasWavesNode
    with MkWavesEntities
    with ApiExtensions
    with ItMatchers
    with DockerExtensions
    with DoubleOps
    with WavesFeeConstants
    with PredefinedAssets
    with PredefinedAccounts
    with DiffMatcher
    with ScorexLogging {

  GenesisConfig.setupAddressScheme()

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor {
    Executors.newFixedThreadPool(10, new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build)
  }

  protected implicit val futureHttpBackend = new LoggingSttpBackend[Future, Nothing](
    AsyncHttpClientFutureBackend.usingConfig(
      new DefaultAsyncHttpClientConfig.Builder()
        .setMaxRequestRetry(1)
        .setReadTimeout(10000)
        .setKeepAlive(false)
        .setRequestTimeout(10000)
        .build()
    )
  )

  protected implicit val tryHttpBackend = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected implicit def toDexExplicitGetOps[F[_]: CanExtract: Functor](self: DexApi[F]): DexApiOps.ExplicitGetDexApiOps[F] = {
    new DexApiOps.ExplicitGetDexApiOps[F](self)
  }

  override implicit def patienceConfig: PatienceConfig = super.patienceConfig.copy(
    timeout = 30.seconds,
    interval = 1.second
  )

  protected def suiteInitialWavesNodeConfig: Config = ConfigFactory.empty()
  protected def suiteInitialDexConfig: Config       = ConfigFactory.empty()

  protected val internalDockerClient: Coeval[com.wavesplatform.dex.it.docker.Docker] = Coeval.evalOnce {
    com.wavesplatform.dex.it.docker.Docker(getClass)
  }

  override protected def dockerClient: com.wavesplatform.dex.it.docker.Docker = internalDockerClient()

  // Waves miner node
  protected val wavesNodeRunConfig: Coeval[Config] = Coeval.evalOnce(GenesisConfig.config)

  protected val wavesNode1Container: Coeval[WavesNodeContainer] = Coeval.evalOnce { createWavesNode("waves-1") }

  private val cachedWavesNode1ApiAddress = CachedData {
    dockerClient.getExternalSocketAddress(wavesNode1Container(), wavesNode1Container().restApiPort)
  }

  protected def wavesNode1Api: NodeApi[Id] = {
    // MonadError can't be implemented for Id
    fp.sync(NodeApi[Try]("integration-test-rest-api", cachedWavesNode1ApiAddress.get()))
  }

  private val cachedWavesNode1NetworkApiAddress = CachedData {
    dockerClient.getInternalSocketAddress(wavesNode1Container(), wavesNode1Container().networkApiPort)
  }

  protected def wavesNode1NetworkApiAddress: InetSocketAddress = cachedWavesNode1NetworkApiAddress.get()

  // Dex server
  protected val dexRunConfig: Coeval[Config] = Coeval.evalOnce {
    dexQueueConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
  }

  protected val dex1Container: Coeval[DexContainer] = Coeval.evalOnce(createDex("dex-1"))

  private val cachedDex1ApiAddress = CachedData {
    dockerClient.getExternalSocketAddress(dex1Container(), dex1Container().restApiPort)
  }

  protected def dex1AsyncApi: DexApi[Future] = DexApi[Future]("integration-test-rest-api", cachedDex1ApiAddress.get())
  protected def dex1Api: DexApi[Id]          = fp.sync(DexApi[Try]("integration-test-rest-api", cachedDex1ApiAddress.get()))

  protected override def invalidateCaches(): Unit = {
    cachedWavesNode1ApiAddress.invalidate()
    cachedDex1ApiAddress.invalidate()
    cachedWavesNode1NetworkApiAddress.invalidate()
  }

  override protected def beforeAll(): Unit = {
    log.debug(s"Perform beforeAll")
    startAndWait(wavesNode1Container(), wavesNode1Api)
    startAndWait(dex1Container(), dex1Api)
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Perform afterAll")
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
    dockerClient.printDebugMessage(formatted)
  }

  protected def createDex(name: String, runConfig: Config = dexRunConfig(), suiteInitialConfig: Config = suiteInitialDexConfig): DexContainer = {
    DexItDocker.createContainer(dockerClient)(name, runConfig, suiteInitialConfig)
  }

  protected def createWavesNode(name: String,
                                runConfig: Config = wavesNodeRunConfig(),
                                suiteInitialConfig: Config = suiteInitialWavesNodeConfig): WavesNodeContainer = {
    WavesIntegrationItDocker.createContainer(dockerClient)(name, runConfig, suiteInitialConfig)
  }

  protected def dexQueueConfig(queueId: Int): Config = {
    Option { System.getenv("KAFKA_SERVER") }.fold { ConfigFactory.empty() } { kafkaServer =>
      ConfigFactory.parseString(s"""waves.dex.events-queue {
                                   |  type = kafka
                                   |  kafka {
                                   |    servers = "$kafkaServer"
                                   |    topic = "dex-$queueId"
                                   |  }
                                   |}""".stripMargin)
    }
  }
}
