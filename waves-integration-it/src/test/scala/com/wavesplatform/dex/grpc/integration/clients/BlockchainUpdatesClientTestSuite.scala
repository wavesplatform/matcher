package com.wavesplatform.dex.grpc.integration.clients

import cats.syntax.option._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.IntegrationSuiteBase
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.{BlockchainUpdatesConversions, DefaultBlockchainUpdatesClient}
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.Implicits._
import com.wavesplatform.dex.grpc.integration.clients.domain.{TransactionWithChanges, WavesNodeEvent}
import com.wavesplatform.dex.grpc.integration.settings.GrpcClientSettings
import com.wavesplatform.dex.it.test.NoStackTraceCancelAfterFailure
import im.mak.waves.transactions.Transaction
import io.grpc.ManagedChannel
import io.grpc.internal.DnsNameResolverProvider
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.execution.Scheduler

import java.util.concurrent.Executors
import scala.collection.immutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class BlockchainUpdatesClientTestSuite extends IntegrationSuiteBase with NoStackTraceCancelAfterFailure {

  private val grpcExecutor = Executors.newCachedThreadPool(
    new ThreadFactoryBuilder()
      .setDaemon(true)
      .setNameFormat("grpc-%d")
      .build()
  )

  implicit private val monixScheduler: Scheduler = monix.execution.Scheduler.cached("monix", 1, 5)

  private lazy val eventLoopGroup = new NioEventLoopGroup

  private lazy val blockchainUpdatesChannel: ManagedChannel =
    GrpcClientSettings(
      target = wavesNode1.blockchainUpdatesExtApiTarget,
      maxHedgedAttempts = 5,
      maxRetryAttempts = 5,
      keepAliveWithoutCalls = true,
      keepAliveTime = 2.seconds,
      keepAliveTimeout = 5.seconds,
      idleTimeout = 1.minute,
      channelOptions = GrpcClientSettings.ChannelOptionsSettings(connectTimeout = 5.seconds)
    ).toNettyChannelBuilder
      .nameResolverFactory(new DnsNameResolverProvider)
      .executor((command: Runnable) => grpcExecutor.execute(command))
      .eventLoopGroup(eventLoopGroup)
      .channelType(classOf[NioSocketChannel])
      .usePlaintext()
      .build

  private lazy val client =
    new DefaultBlockchainUpdatesClient(eventLoopGroup, blockchainUpdatesChannel, monixScheduler)(ExecutionContext.fromExecutor(grpcExecutor))

  implicit override def patienceConfig: PatienceConfig = super.patienceConfig.copy(
    timeout = 1.minute,
    interval = 1.second
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
  }

  "StateUpdate.pessimisticPortfolio" - {
    "ExchangeTransaction with one trader - returns an expected value" in {
      val priceCoef = BigInt(10).pow(8 + IssueUsdTx.decimals() - 8).toLong
      val exchangeTx = mkExchange(alice, alice, wavesUsdPair, 3_00000000, 2 * priceCoef, matcher = matcher)

      val pp = sendAndWaitTxFromStream(exchangeTx).pessimisticPortfolios
      withClue(s"pp: ${pp.mkString(", ")}: ") {
        pp should matchTo(Map(
          alice.toAddress -> Map[Asset, Long](
            Asset.Waves -> -(3_00000000 + 2 * matcherFee), // sell 3 WAVES and spend fees for both orders
            usd -> -3 * 200 // buy 3 WAVES for 200 cents each
          )
        ))
      }
    }
  }

  override protected def afterAll(): Unit = {
    Await.ready(client.close(), 10.seconds)
    super.afterAll()
    grpcExecutor.shutdownNow()
  }

  private def sendAndWaitTxFromStream(tx: Transaction): TransactionWithChanges = {
    val pbTxId = ByteString.copyFrom(tx.id().bytes())

    val receivedTxFuture = client.blockchainEvents.stream
      .flatMapIterable(evt => immutable.Iterable.from(evt.update.flatMap(BlockchainUpdatesConversions.toEvent)))
      .map { evt =>
        log.debug(s"Got $evt")
        evt match {
          case evt: WavesNodeEvent.Appended if evt.block.confirmedTxs.contains(pbTxId) => evt.block.confirmedTxs.values.head.some
          case _ =>
            client.blockchainEvents.requestNext()
            none
        }
      }
      .collect { case Some(x) => x }
      .firstL
      .runToFuture

    val startHeight = wavesNode1.api.currentHeight
    broadcastAndAwait(tx)

    client.blockchainEvents.startFrom(startHeight)
    val receivedTx = wait(receivedTxFuture)
    client.blockchainEvents.stop()
    receivedTx
  }

  private def wait[T](f: => Future[T]): T = Await.result(f, 1.minute)

}
