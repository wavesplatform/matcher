package com.wavesplatform.dex.grpc.integration

import akka.actor.ActorSystem
import cats.syntax.option._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV3}
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.DefaultBlockchainUpdatesClient
import com.wavesplatform.dex.grpc.integration.clients.combined.{AkkaCombinedStream, CombinedStream, CombinedWavesBlockchainClient}
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.settings.{GrpcClientSettings, WavesBlockchainClientSettings}
import com.wavesplatform.dex.grpc.integration.tool.RestartableManagedChannel
import com.wavesplatform.events.protobuf.BlockchainUpdated
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.BooleanCancelable
import org.scalatest.OptionValues

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicLong
import scala.collection.immutable
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.Random
import scala.util.chaining._

final class TransactionsOrderingTestSuite extends IntegrationSuiteBase with OptionValues {

  private val txsNumber = 20

  private val grpcExecutor1 = Executors.newCachedThreadPool(
    new ThreadFactoryBuilder()
      .setDaemon(true)
      .setNameFormat("grpc1-%d")
      .build()
  )

  private val grpcExecutor2 = Executors.newCachedThreadPool(
    new ThreadFactoryBuilder()
      .setDaemon(true)
      .setNameFormat("grpc2-%d")
      .build()
  )

  private val actorSystem = ActorSystem()

  implicit private val monixScheduler: Scheduler = monix.execution.Scheduler.cached("monix", 1, 5)

  private lazy val eventLoopGroup = new NioEventLoopGroup

  private val keepAliveTime = 10.seconds
  private val keepAliveTimeout = 5.seconds

  private lazy val grpcSettings = GrpcClientSettings(
    target = s"127.0.0.1:${wavesNode1.blockchainUpdatesExtGrpcApiAddress.getPort}",
    maxHedgedAttempts = 5,
    maxRetryAttempts = 5,
    keepAliveWithoutCalls = true,
    keepAliveTime = keepAliveTime,
    keepAliveTimeout = keepAliveTimeout,
    idleTimeout = 1.day,
    channelOptions = GrpcClientSettings.ChannelOptionsSettings(connectTimeout = 5.seconds),
    noDataTimeout = 5.minutes
  )

  private lazy val blockchainUpdatesChannel: RestartableManagedChannel =
    new RestartableManagedChannel(() =>
      grpcSettings.toNettyChannelBuilder
        .executor((command: Runnable) => grpcExecutor1.execute(command))
        .eventLoopGroup(eventLoopGroup)
        .channelType(classOf[NioSocketChannel])
        .usePlaintext()
        .build
    )

  private lazy val client =
    new DefaultBlockchainUpdatesClient(eventLoopGroup, blockchainUpdatesChannel, monixScheduler, grpcSettings.noDataTimeout)(
      ExecutionContext.fromExecutor(grpcExecutor1)
    )

  private lazy val bchClient = CombinedWavesBlockchainClient(
    WavesBlockchainClientSettings(
      grpcSettings.copy(target = s"127.0.0.1:${wavesNode1.matcherExtGrpcApiAddress.getPort}"),
      grpcSettings,
      100.millis,
      100,
      CombinedWavesBlockchainClient.Settings(100, 12, CombinedStream.Settings(1.seconds), SynchronizedPessimisticPortfolios.Settings(50))
    ),
    matcherPublicKey = matcher.publicKey,
    monixScheduler = monixScheduler,
    grpcExecutionContext = ExecutionContext.fromExecutor(grpcExecutor2),
    mkCombinedStream = (meClient, buClient) =>
      new AkkaCombinedStream(
        CombinedStream.Settings(1.seconds),
        buClient.blockchainEvents,
        meClient.utxEvents
      )(actorSystem, monixScheduler)
  )

  "TransactionsOrderingTestSuite" - {

    "exchange transactions should be ordered by timestamp after packing in a block" in {
      val startHeight = wavesNode1.api.currentHeightOrig.height
      val tsAtomic = new AtomicLong(System.currentTimeMillis())

      val txs = (1 to txsNumber).map { i =>
        val amount = Random.between(0.001, 5.99)
        val price = Random.between(1.1, 10.3)
        val buyFee = Random.between(0.00001, 1.3)
        val sellFee = Random.between(0.00001, 1.3)
        val buyFeeAsset = if (i % 2 == 0) Asset.Waves else usdn
        val sellFeeAsset = if (i % 3 == 0) Asset.Waves else usdn

        mkExchangeTx(
          alice,
          bob,
          wavesUsdnPair,
          amount.waves,
          price.usdn,
          tsAtomic.getAndIncrement(),
          buyFee.waves,
          sellFee.waves,
          buyOrderFeeAsset = buyFeeAsset,
          sellOrderFeeAsset = sellFeeAsset
        ).tap { v =>
          log.info(s"$i tx ${v.id().base58}")
        }
      }

      Random.shuffle(txs).foreach(bchClient.checkedBroadcastTx)

      val result = waitTxsFromStream(startHeight, txsNumber)
      txs.size shouldBe result.size
      result.zip(txs.map(_.id())).foreach {
        case (txFromBlock, tx) =>
          ByteStr.fromByteArray(txFromBlock.toByteArray).base58 shouldBe tx.base58
      }
    }
  }

  private def waitTxsFromStream(startHeight: Int, txNumber: Int): List[ByteString] = {
    val cancellable = BooleanCancelable()

    client.blockchainEvents.systemStream
      .takeWhileNotCanceled(cancellable)
      .doOnNext(evt => Task(log.info(s"System event: $evt")))
      .lastOptionL.runToFuture

    val txsFuture = client.blockchainEvents.stream
      .flatMapIterable(evt => immutable.Iterable.from(evt.update))
      .map { evt =>
        evt.update match {
          case BlockchainUpdated.Update.Append(appended) if appended.transactionIds.size >= txNumber =>
            Some(appended.transactionIds)
          case _ =>
            client.blockchainEvents.requestNext()
            none
        }
      }
      .collect { case Some(x) => x }
      .firstL
      .runToFuture

    client.blockchainEvents.startFrom(startHeight)
    val txs = txsFuture.futureValue.iterator.toList
    client.blockchainEvents.stop()
    Thread.sleep(1000L)
    cancellable.cancel()

    txs
  }

  private def mkExchangeTx(
    buyOrderOwner: KeyPair,
    sellOrderOwner: KeyPair,
    pair: AssetPair,
    amount: Long,
    price: Long,
    timestamp: Long,
    buyOrderFee: Long,
    sellOrderFee: Long,
    buyOrderFeeAsset: Asset,
    sellOrderFeeAsset: Asset,
    matcherFee: Long = matcherFee
  )(implicit assetDecimals: Map[Asset, Int]): ExchangeTransaction = {

    val buyOrder = mkOrder(
      buyOrderOwner,
      pair,
      OrderType.BUY,
      amount,
      price,
      buyOrderFee,
      matcher = matcher,
      feeAsset = buyOrderFeeAsset,
      version = 3.toByte
    )
    val sellOrder = mkOrder(
      sellOrderOwner,
      pair,
      OrderType.SELL,
      amount,
      price,
      sellOrderFee,
      matcher = matcher,
      feeAsset = sellOrderFeeAsset,
      version = 3.toByte
    )

    ExchangeTransactionV3
      .mkSigned(
        amountAssetDecimals = assetDecimals(buyOrder.assetPair.amountAsset),
        priceAssetDecimals = assetDecimals(buyOrder.assetPair.priceAsset),
        matcher = matcher,
        buyOrder = buyOrder,
        sellOrder = sellOrder,
        amount = amount,
        price = price,
        buyOrder.matcherFee,
        sellOrder.matcherFee,
        matcherFee,
        timestamp
      ).transaction
  }

  override def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdnTx)
    broadcastAndAwait(mkTransfer(alice, matcher, defaultAssetQuantity / 4, usdn), mkTransfer(alice, bob, defaultAssetQuantity / 4, usdn))
  }

  override protected def afterAll(): Unit = {
    Await.ready(client.close(), 10.seconds)
    super.afterAll()
    grpcExecutor1.shutdownNow()
    grpcExecutor2.shutdownNow()
  }

}
