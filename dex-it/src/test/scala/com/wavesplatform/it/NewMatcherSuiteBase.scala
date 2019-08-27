package com.wavesplatform.it

import java.net.InetSocketAddress
import java.nio.file.Paths
import java.util.concurrent.Executors

import cats.Id
import cats.instances.try_._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp._
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.it.api.{DexApi, HasWaitReady, LoggingSttpBackend, NodeApi}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, DockerContainer, WavesNodeContainer}
import com.wavesplatform.it.sync.matcherFee
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Try

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with TestUtils with ScorexLogging {

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newCachedThreadPool(new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build()))

  protected implicit val tryHttpBackend = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected val dockerClient: Coeval[docker.Docker] = Coeval.evalOnce(docker.Docker(getClass))

  // Waves miner node

  protected def wavesNode1Config: Config = DexTestConfig.containerConfig("waves-1")
  protected val wavesNode1Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-1", wavesNode1Config.resolve())
  }

  // TODO move to container
  protected def wavesNode1Api: NodeApi[cats.Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(wavesNode1Container(), wavesNode1Config.getInt("waves.rest-api.port"))
    fp.sync(NodeApi[Try]("integration-test-rest-api", apiAddress))
  }

  protected def wavesNode1NetworkApiAddress: InetSocketAddress =
    dockerClient().getInternalSocketAddress(wavesNode1Container(), wavesNode1Config.getInt("waves.network.port"))

  // Dex server

  protected def dex1Config: Config                 = DexTestConfig.containerConfig("dex-1")
  protected def dex1NodeContainer: DockerContainer = wavesNode1Container()
  protected val dex1Container: Coeval[DexContainer] = Coeval.evalOnce {
    val grpcAddr = dockerClient().getInternalSocketAddress(dex1NodeContainer, dex1NodeContainer.config.getInt("waves.dex.grpc.integration.port"))
    val wavesNodeGrpcConfig = ConfigFactory
      .parseString(s"""waves.dex.waves-node-grpc {
                      |  host = ${grpcAddr.getAddress.getHostAddress}
                      |  port = ${grpcAddr.getPort}
                      |}""".stripMargin)
    // TODO Has a greater priority than local.conf!
    val config = DexTestConfig.updatedMatcherConfig.withFallback(wavesNodeGrpcConfig).withFallback(dex1Config).resolve()
    dockerClient().createDex("dex-1", config)
  }

  protected def dex1Api: DexApi[Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(dex1Container(), dex1Config.getInt("waves.dex.rest-api.port"))
    fp.sync(DexApi[Try]("integration-test-rest-api", apiAddress))
  }

  protected def allContainers: List[DockerContainer] = List(wavesNode1Container, dex1Container).map(x => x())
  protected def allApis: List[HasWaitReady[cats.Id]] = List(wavesNode1Api, dex1Api)

  override protected def beforeAll(): Unit = {
    log.debug(s"Doing beforeAll")
    super.beforeAll()
    allContainers.foreach(dockerClient().start)
    allApis.foreach(_.waitReady)
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

trait TestUtils {
  this: NewMatcherSuiteBase =>

  /**
    * @param matcherFeeAssetId If specified IssuedAsset, the version will be automatically set to 3
    */
  protected def prepareOrder(owner: KeyPair,
                             matcher: PublicKey,
                             pair: AssetPair,
                             orderType: OrderType,
                             amount: Long,
                             price: Long,
                             matcherFee: Long = matcherFee,
                             matcherFeeAssetId: Asset = Waves,
                             timestamp: Long = System.currentTimeMillis(),
                             timeToLive: Duration = 30.days - 1.seconds,
                             version: Byte = 1): Order =
    if (matcherFeeAssetId == Waves)
      Order(
        sender = owner,
        matcher = matcher,
        pair = pair,
        orderType = orderType,
        amount = amount,
        price = price,
        timestamp = timestamp,
        expiration = timestamp + timeToLive.toMillis,
        matcherFee = matcherFee,
        version = version,
      )
    else
      Order(
        sender = owner,
        matcher = matcher,
        pair = pair,
        orderType = orderType,
        amount = amount,
        price = price,
        timestamp = timestamp,
        expiration = timestamp + timeToLive.toMillis,
        matcherFee = matcherFee,
        version = version,
        matcherFeeAssetId = matcherFeeAssetId
      )

  protected def issueAssets(txs: IssueTransaction*): Unit = {
    txs.map(wavesNode1Api.broadcast)
    txs.foreach(tx => wavesNode1Api.waitForTransaction(tx.id()))
  }

  protected def restartContainer(container: DockerContainer, api: HasWaitReady[cats.Id]): Unit = {
    dockerClient().stop(container)
    dockerClient().start(container)
    api.waitReady
  }

  protected def replaceLocalConfig(container: DockerContainer, config: Config): Unit =
    replaceLocalConfig(
      container,
      config
        .resolve()
        .root()
        .render(
          ConfigRenderOptions
            .concise()
            .setOriginComments(false)
            .setComments(false)
            .setFormatted(true)
            .setJson(false)
        )
    )

  protected def replaceLocalConfig(container: DockerContainer, content: String): Unit = {
    val path = Paths.get(container.basePath, "local.conf")
    log.trace(s"Replacing '$path' of $container by:\n$content")
    dockerClient().writeFile(container, path, content)
  }
}
