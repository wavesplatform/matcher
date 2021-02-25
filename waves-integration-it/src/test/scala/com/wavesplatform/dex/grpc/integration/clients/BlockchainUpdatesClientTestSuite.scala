package com.wavesplatform.dex.grpc.integration.clients

import cats.syntax.option._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.IntegrationSuiteBase
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.{BlockchainUpdatesConversions, DefaultBlockchainUpdatesClient}
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.Implicits._
import com.wavesplatform.dex.grpc.integration.clients.domain.{TransactionWithChanges, WavesNodeEvent}
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.settings.GrpcClientSettings
import com.wavesplatform.dex.it.api.HasToxiProxy
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.test.NoStackTraceCancelAfterFailure
import im.mak.waves.transactions.Transaction
import io.grpc.ManagedChannel
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.BooleanCancelable

import java.util.concurrent.Executors
import scala.collection.immutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class BlockchainUpdatesClientTestSuite extends IntegrationSuiteBase with HasToxiProxy with NoStackTraceCancelAfterFailure {

  implicit override def patienceConfig: PatienceConfig = super.patienceConfig.copy(
    timeout = 1.minute,
    interval = 1.second
  )

  private val grpcExecutor = Executors.newCachedThreadPool(
    new ThreadFactoryBuilder()
      .setDaemon(true)
      .setNameFormat("grpc-%d")
      .build()
  )

  implicit private val monixScheduler: Scheduler = monix.execution.Scheduler.cached("monix", 1, 5)

  private lazy val blockchainUpdatesProxy =
    toxiContainer.getProxy(wavesNode1.underlying.container, WavesNodeContainer.blockchainUpdatesGrpcExtensionPort)

  private lazy val eventLoopGroup = new NioEventLoopGroup

  private val keepAliveTime = 2.seconds
  private val keepAliveTimeout = 5.seconds

  private lazy val blockchainUpdatesChannel: ManagedChannel =
    GrpcClientSettings(
      target = s"127.0.0.1:${blockchainUpdatesProxy.getProxyPort}",
      maxHedgedAttempts = 5,
      maxRetryAttempts = 5,
      keepAliveWithoutCalls = true,
      keepAliveTime = keepAliveTime,
      keepAliveTimeout = keepAliveTimeout,
      idleTimeout = 1.day,
      channelOptions = GrpcClientSettings.ChannelOptionsSettings(connectTimeout = 5.seconds)
    ).toNettyChannelBuilder
      .executor((command: Runnable) => grpcExecutor.execute(command))
      .eventLoopGroup(eventLoopGroup)
      .channelType(classOf[NioSocketChannel])
      .usePlaintext()
      .build

  private lazy val client =
    new DefaultBlockchainUpdatesClient(eventLoopGroup, blockchainUpdatesChannel, monixScheduler)(ExecutionContext.fromExecutor(grpcExecutor))

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

  private val events = client.blockchainEvents.stream

  "Bugs" - {
    "DEX-1084 No updates from Blockchain updates" in {
      val cancellable = BooleanCancelable()
      @volatile var lastStatus: SystemEvent = SystemEvent.BecameReady

      val eventsF = events
        .takeWhileNotCanceled(cancellable)
        .doOnNext(_ => Task(client.blockchainEvents.requestNext()))
        .doOnComplete(Task(log.info("events completed")))
        .toListL.runToFuture

      client.blockchainEvents.systemStream
        .takeWhileNotCanceled(cancellable)
        .doOnNext { evt =>
          Task {
            lastStatus = evt
            log.info(s"System event: $evt")
          }
        }
        .doOnComplete(Task(log.info("system events completed")))
        .lastOptionL.runToFuture

      val startHeight = wavesNode1.api.currentHeight
      client.blockchainEvents.startFrom(startHeight)

      step("transfer1")
      val transfer1 = mkTransfer(alice, bob, 1, Asset.Waves)
      broadcastAndAwait(transfer1)

      step("Cut connection to gRPC extension")
      blockchainUpdatesProxy.setConnectionCut(true)

      val transfer2 = mkTransfer(bob, matcher, 1, Asset.Waves)
      broadcastAndAwait(transfer2)

      Thread.sleep((keepAliveTime + keepAliveTimeout + 3.seconds).toMillis)

      step("Enable connection to gRPC extension")
      blockchainUpdatesProxy.setConnectionCut(false)

      // Connection should be closed, restore it
      lastStatus shouldBe SystemEvent.Stopped
      client.blockchainEvents.startFrom(startHeight)

      Thread.sleep(5.seconds.toMillis)

      cancellable.cancel()
      val xs = Await.result(eventsF, 1.minute).map { evt =>
        val event = BlockchainUpdatesConversions.toEvent(evt.getUpdate)
        log.debug(s"Got $event")
        event.flatMap {
          case WavesNodeEvent.Appended(block) => block.confirmedTxs.map(_._1.toVanilla).toList.some
          case _ => none
        }
      }
      client.blockchainEvents.stop()

      val gotTxIds = xs.flatMap {
        case None => List.empty
        case Some(txIds) => txIds.map(_.base58)
      }

      withClue("transfer1: ") {
        gotTxIds should contain(transfer1.id().base58)
      }

      withClue("transfer2: ") {
        gotTxIds should contain(transfer2.id().base58)
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
    val cancellable = BooleanCancelable()

    client.blockchainEvents.systemStream
      .takeWhileNotCanceled(cancellable)
      .doOnNext(evt => Task(log.info(s"System event: $evt")))
      .lastOptionL.runToFuture

    val receivedTxFuture = events
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

    Thread.sleep(1000)
    cancellable.cancel()

    receivedTx
  }

  private def wait[T](f: => Future[T]): T = Await.result(f, 1.minute)

}
