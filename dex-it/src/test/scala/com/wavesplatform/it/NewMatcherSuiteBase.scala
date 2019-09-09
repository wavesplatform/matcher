package com.wavesplatform.it

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.concurrent.{Executors, ThreadLocalRandom}

import cats.Id
import cats.instances.future._
import cats.instances.try_._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.account.{Address, AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.{DexApi, HasWaitReady, LoggingSttpBackend, MatcherError, MatcherState, NodeApi, OrderBookHistoryItem}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.docker.{DexContainer, DockerContainer, WavesNodeContainer}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.test.FailWith
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV2, SetAssetScriptTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV2, LeaseTransaction, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionV2}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.scalatest._
import org.scalatest.matchers.Matcher

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

abstract class NewMatcherSuiteBase extends FreeSpec with Matchers with CancelAfterFailure with BeforeAndAfterAll with TestUtils with ScorexLogging {

  protected def suiteInitialWavesNodeConfig: Config = ConfigFactory.empty()
  protected def suiteInitialDexConfig: Config       = ConfigFactory.empty()

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'Y'.toByte
  }

  protected implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newCachedThreadPool(new ThreadFactoryBuilder().setNameFormat(s"${getClass.getSimpleName}-%d").setDaemon(true).build()))

  protected implicit val futureHttpBackend = new LoggingSttpBackend[Future, Nothing](AsyncHttpClientFutureBackend())
  protected implicit val tryHttpBackend    = new LoggingSttpBackend[Try, Nothing](TryHttpURLConnectionBackend())

  protected val dockerClient: Coeval[docker.Docker] = Coeval.evalOnce(docker.Docker(getClass))

  // Waves miner node
  protected val wavesNodeRunConfig: Coeval[Config] = Coeval.evalOnce(DexTestConfig.genesisConfig)

  protected val wavesNode1Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-1", wavesNodeRunConfig(), suiteInitialWavesNodeConfig)
  }

  protected def wavesNode1Api: NodeApi[cats.Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(wavesNode1Container(), wavesNode1Container().restApiPort)
    fp.sync(NodeApi[Try]("integration-test-rest-api", apiAddress))
  }

  protected def wavesNode1NetworkApiAddress: InetSocketAddress =
    dockerClient().getInternalSocketAddress(wavesNode1Container(), wavesNode1Container().networkApiPort)

  // Dex server
  protected val dexRunConfig: Coeval[Config] = Coeval.evalOnce {
    dexQueueConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
      .withFallback(dexWavesGrpcConfig(wavesNode1Container()))
      .withFallback(DexTestConfig.updatedMatcherConfig)
  }

  protected val dex1Container: Coeval[DexContainer] = Coeval.evalOnce {
    dockerClient().createDex("dex-1", dexRunConfig(), suiteInitialDexConfig)
  }

  private def dex1ApiAddress                 = dockerClient().getExternalSocketAddress(dex1Container(), dex1Container().restApiPort)
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

    waves.foreach(dockerClient().start)
    wavesApi.foreach(_.waitReady)

    dex.foreach(dockerClient().start)
    dexApi.foreach(_.waitReady)
  }

  override protected def afterAll(): Unit = {
    log.debug(s"Doing afterAll")
    dockerClient().close()
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
    try allContainers.foreach(x => dockerClient().printDebugMessage(x, formatted))
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
    val grpcAddr = dockerClient().getInternalSocketAddress(target, target.grpcApiPort)
    ConfigFactory
      .parseString(s"""waves.dex {
                      |  waves-node-grpc {
                      |    host = ${grpcAddr.getAddress.getHostAddress}
                      |    port = ${grpcAddr.getPort}
                      |  }
                      |}""".stripMargin)
  }
}

trait TestUtils {
  this: NewMatcherSuiteBase =>

  protected def orderVersion = (ThreadLocalRandom.current().nextInt(3) + 1).toByte

  /**
    * @param matcherFeeAssetId If specified IssuedAsset, the version will be automatically set to 3
    */
  protected def mkOrder(owner: KeyPair,
                        pair: AssetPair,
                        orderType: OrderType,
                        amount: Long,
                        price: Long,
                        matcherFee: Long = matcherFee,
                        matcherFeeAssetId: Asset = Waves,
                        ts: Long = System.currentTimeMillis(),
                        ttl: Duration = 30.days - 1.seconds,
                        version: Byte = orderVersion,
                        matcher: PublicKey = DexTestConfig.matcher): Order =
    if (matcherFeeAssetId == Waves)
      Order(
        sender = owner,
        matcher = matcher,
        pair = pair,
        orderType = orderType,
        amount = amount,
        price = price,
        timestamp = ts,
        expiration = ts + ttl.toMillis,
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
        timestamp = ts,
        expiration = ts + ttl.toMillis,
        matcherFee = matcherFee,
        version = 3,
        matcherFeeAssetId = matcherFeeAssetId
      )

  protected def mkTransfer(sender: KeyPair,
                           recipient: Address,
                           amount: Long,
                           asset: Asset,
                           feeAmount: Long = minFee,
                           feeAsset: Asset = Waves,
                           timestamp: Long = System.currentTimeMillis()): TransferTransaction =
    TransferTransactionV2
      .selfSigned(
        assetId = asset,
        sender = sender,
        recipient = recipient,
        amount = amount,
        timestamp = timestamp,
        feeAssetId = feeAsset,
        feeAmount = feeAmount,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

  protected def mkLease(sender: KeyPair,
                        recipient: Address,
                        amount: Long,
                        fee: Long = leasingFee,
                        timestamp: Long = System.currentTimeMillis()): LeaseTransaction =
    LeaseTransactionV2
      .selfSigned(
        sender = sender,
        amount = amount,
        fee = fee,
        timestamp = timestamp,
        recipient = recipient
      )
      .explicitGet()

  protected def mkLeaseCancel(sender: KeyPair,
                              leaseId: ByteStr,
                              fee: Long = leasingFee,
                              timestamp: Long = System.currentTimeMillis()): LeaseCancelTransaction =
    LeaseCancelTransactionV2
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = sender,
        leaseId = leaseId,
        fee = fee,
        timestamp = timestamp
      )
      .explicitGet()

  protected def mkIssue(issuer: KeyPair,
                        name: String,
                        quantity: Long,
                        decimals: StatusCode = 8,
                        fee: Long = issueFee,
                        script: Option[Script] = None,
                        reissuable: Boolean = false,
                        timestamp: Long = System.currentTimeMillis()): IssueTransaction =
    IssueTransactionV2
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = issuer,
        name = name.getBytes(),
        description = s"$name asset".getBytes(StandardCharsets.UTF_8),
        quantity = quantity,
        decimals = decimals.toByte,
        reissuable = false,
        script = script,
        fee = fee,
        timestamp = timestamp
      )
      .explicitGet()

  protected def mkSetAccountScript(accountOwner: KeyPair,
                                   script: Option[Script],
                                   fee: Long = setScriptFee,
                                   ts: Long = System.currentTimeMillis()): SetScriptTransaction =
    SetScriptTransaction
      .selfSigned(
        sender = accountOwner,
        script = script,
        fee = fee,
        timestamp = ts
      )
      .explicitGet()

  protected def mkSetAccountScriptText(accountOwner: KeyPair,
                                       scriptText: Option[String],
                                       fee: Long = setScriptFee,
                                       ts: Long = System.currentTimeMillis()): SetScriptTransaction = {
    val script = scriptText.map { x =>
      ScriptCompiler.compile(x.stripMargin, ScriptEstimatorV2).explicitGet()._1
    }

    mkSetAccountScript(accountOwner, script, fee, ts)
  }

  protected def mkSetAssetScript(assetOwner: KeyPair,
                                 asset: IssuedAsset,
                                 script: Option[Script],
                                 fee: Long = setAssetScriptFee,
                                 ts: Long = System.currentTimeMillis()): SetAssetScriptTransaction =
    SetAssetScriptTransaction
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = assetOwner,
        asset = asset,
        script = script,
        fee = fee,
        timestamp = ts
      )
      .explicitGet()

  protected def mkSetAssetScriptText(assetOwner: KeyPair,
                                     asset: IssuedAsset,
                                     scriptText: Option[String],
                                     fee: Long = setAssetScriptFee,
                                     ts: Long = System.currentTimeMillis()): SetAssetScriptTransaction = {
    val script = scriptText.map { x =>
      ScriptCompiler.compile(x.stripMargin, ScriptEstimatorV2).explicitGet()._1
    }

    mkSetAssetScript(assetOwner, asset, script, fee, ts)
  }

  protected def broadcast(txs: Transaction*): Unit = {
    txs.map(wavesNode1Api.broadcast)
    txs.foreach(tx => wavesNode1Api.waitForTransaction(tx.id()))
  }

  protected def restartContainer(container: DockerContainer, api: HasWaitReady[cats.Id]): Unit = {
    dockerClient().stop(container)
    dockerClient().start(container)
    api.waitReady
  }

  protected def replaceSuiteConfig(container: DockerContainer, config: Config): Unit =
    replaceSuiteConfig(
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

  protected def replaceSuiteConfig(container: DockerContainer, content: String): Unit = {
    val path = Paths.get(container.basePath, "suite.conf")
    log.trace(s"Replacing '$path' of $container by:\n$content")
    dockerClient().writeFile(container, path, content)
  }

  protected def waitForOrderAtNode(orderId: Order.Id,
                                   dexApi: DexApi[Id] = dex1Api,
                                   wavesNodeApi: NodeApi[Id] = wavesNode1Api): Id[ExchangeTransaction] = {
    val tx = dexApi.waitForTransactionsByOrder(orderId, 1).head
    wavesNodeApi.waitForTransaction(tx.id())
    tx
  }

  protected def matcherState(assetPairs: Seq[AssetPair],
                             orders: IndexedSeq[Order],
                             accounts: Seq[KeyPair],
                             dexApi: DexApi[Id] = dex1Api): Id[MatcherState] = {
    val offset               = dexApi.currentOffset
    val snapshots            = dexApi.allSnapshotOffsets
    val orderBooks           = assetPairs.map(x => (x, (dexApi.orderBook(x), dexApi.orderBookStatus(x))))
    val orderStatuses        = orders.map(x => x.idStr() -> dexApi.orderStatus(x))
    val reservedBalances     = accounts.map(x => x -> dexApi.reservedBalance(x))
    val accountsOrderHistory = accounts.flatMap(a => assetPairs.map(p => a -> p))
    val orderHistory = accountsOrderHistory.map {
      case (account, pair) => (account, pair, dexApi.orderHistoryByPair(account, pair))
    }

    val orderHistoryMap = orderHistory
      .groupBy(_._1) // group by accounts
      .map {
        case (account, xs) =>
          val assetPairHistory = xs.groupBy(_._2).map { // group by asset pair
            case (assetPair, historyRecords) => assetPair -> historyRecords.flatMap(_._3) // same as historyRecords.head._3
          }

          account -> (TreeMap.empty[AssetPair, Seq[OrderBookHistoryItem]] ++ assetPairHistory)
      }

    clean {
      api.MatcherState(offset,
                       TreeMap(snapshots.toSeq: _*),
                       TreeMap(orderBooks: _*),
                       TreeMap(orderStatuses: _*),
                       TreeMap(reservedBalances: _*),
                       TreeMap(orderHistoryMap.toSeq: _*))
    }
  }

  private def clean(x: MatcherState): MatcherState = x.copy(
    orderBooks = x.orderBooks.map { case (k, v) => k -> v.copy(_1 = v._1.copy(timestamp = 0L)) }
  )

  private implicit val assetPairOrd: Ordering[AssetPair] = Ordering.by[AssetPair, String](_.key)
  private implicit val keyPairOrd: Ordering[KeyPair]     = Ordering.by[KeyPair, String](_.stringRepr)

  def failWith(errorCode: Int): Matcher[Either[MatcherError, Any]]                      = new FailWith(errorCode)
  def failWith(errorCode: Int, messagePart: String): Matcher[Either[MatcherError, Any]] = new FailWith(errorCode, Some(messagePart))
  def failWith(errorCode: Int, containsParams: MatcherError.Params): Matcher[Either[MatcherError, Any]] =
    new FailWith(errorCode, None, containsParams)
}
