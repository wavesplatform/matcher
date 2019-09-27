package com.wavesplatform.dex

import java.io.File
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.{AskTimeoutException, ask, gracefulStop}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import cats.data.NonEmptyList
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.{ApiRoute, CompositeHttpService => _}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.database._
import com.wavesplatform.dex.Matcher.Status
import com.wavesplatform.dex.api.http.CompositeHttpService
import com.wavesplatform.dex.api.{MatcherApiRoute, MatcherApiRouteV1, OrderBookSnapshotHttpCache}
import com.wavesplatform.dex.cache.{AssetDecimalsCache, BalancesCache, RateCache}
import com.wavesplatform.dex.db.{AccountStorage, AssetPairsDB, OrderBookSnapshotDB, OrderDB}
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.grpc.integration.DEXClient
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.history.HistoryRouter
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.market._
import com.wavesplatform.dex.model.MatcherModel.{Denormalization, Normalization}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue._
import com.wavesplatform.dex.settings.{MatcherSettings, MatchingRules, RawMatchingRules}
import com.wavesplatform.extensions.Extension
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{ErrorStartingMatcher, NTP, ScorexLogging, forceStopApplication}
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.reactive.Observer
import mouse.any._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Matcher(settings: MatcherSettings, gRPCExtensionClient: DEXClient)(implicit val actorSystem: ActorSystem, val materializer: ActorMaterializer)
    extends Extension
    with ScorexLogging {

  import actorSystem.dispatcher
  import gRPCExtensionClient.{monixScheduler, grpcExecutionContext, wavesBlockchainAsyncClient, wavesBlockchainSyncClient}

  private val time = new NTP(settings.ntpServer)

  private val matcherKeyPair = {
    val r = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet()
    log.info(s"The DEX's public key: ${Base58.encode(r.publicKey.arr)}, account address: ${r.publicKey.toAddress.stringRepr}")
    r
  }

  private def matcherPublicKey: PublicKey = matcherKeyPair

  private val status: AtomicReference[Status] = new AtomicReference(Status.Starting)

  private val blacklistedAssets: Set[IssuedAsset] = settings.blacklistedAssets
    .map { assetId =>
      AssetPair.extractAssetId(assetId) match {
        case Success(Waves)          => throw new IllegalArgumentException("Can't blacklist the main coin")
        case Success(a: IssuedAsset) => a
        case Failure(e)              => throw new IllegalArgumentException("Can't parse asset id", e)
      }
    }

  private val pairBuilder    = new AssetPairBuilder(settings, wavesBlockchainSyncClient.assetDescription, blacklistedAssets)
  private val orderBookCache = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot](1000, 0.9f, 10)

  private def hasMatcherAccountScript = wavesBlockchainSyncClient.hasScript(matcherKeyPair)

  private val transactionCreator = {
    new ExchangeTransactionCreator(matcherKeyPair,
                                   settings,
                                   hasMatcherAccountScript,
                                   wavesBlockchainSyncClient.hasScript,
                                   wavesBlockchainSyncClient.isFeatureActivated)
  }

  private val orderBooks         = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val rawMatchingRules   = new ConcurrentHashMap[AssetPair, RawMatchingRules]
  private val assetDecimalsCache = new AssetDecimalsCache(wavesBlockchainSyncClient.assetDescription)
  private val balancesCache      = new BalancesCache(wavesBlockchainAsyncClient.spendableBalance)(grpcExecutionContext)

  /** Updates balances cache by balances stream */
  wavesBlockchainAsyncClient.spendableBalanceChanges.subscribe {
    new Observer[SpendableBalanceChanges] {
      override def onNext(elem: SpendableBalanceChanges): Future[Ack] = { balancesCache.batchUpsert(elem); Continue }
      override def onComplete(): Unit                                 = log.info("Balance changes stream completed!")
      override def onError(ex: Throwable): Unit                       = log.warn(s"Error while listening to the balance changes stream occurred: ${ex.getMessage}")
    }
  }(monixScheduler)

  private val orderBooksSnapshotCache =
    new OrderBookSnapshotHttpCache(
      settings.orderBookSnapshotHttpCache,
      time,
      assetDecimalsCache.get,
      p => Option(orderBookCache.get(p))
    )

  private val marketStatuses = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook.AggregatedSnapshot): Unit = {
    orderBookCache.put(assetPair, newSnapshot)
    orderBooksSnapshotCache.invalidate(assetPair)
  }

  private def normalize(assetPair: AssetPair)(rawMatchingRules: RawMatchingRules): MatchingRules =
    MatchingRules(
      rawMatchingRules.startOffset,
      normalizedTickSize = Normalization.normalizePrice(
        rawMatchingRules.tickSize,
        assetPair,
        MatcherModel.getPairDecimals(assetPair, assetDecimalsCache.get)
      )
    )

  private def denormalizeTickSize(assetPair: AssetPair, normalizedTickSize: Long): Double =
    Denormalization.denormalizePrice(normalizedTickSize, assetPair, x => assetDecimalsCache.get(x))

  private def convert(assetPair: AssetPair, matchingRules: MatchingRules): RawMatchingRules =
    RawMatchingRules(
      startOffset = matchingRules.startOffset,
      tickSize = denormalizeTickSize(assetPair, matchingRules.normalizedTickSize)
    )

  private def matchingRules(assetPair: AssetPair): NonEmptyList[RawMatchingRules] = {
    lazy val default = convert(assetPair, MatchingRules.Default)
    val xs           = settings.matchingRules.getOrElse(assetPair, NonEmptyList.one[RawMatchingRules](default))
    if (xs.head.startOffset == 0) xs else default :: xs
  }

  private def orderBookProps(assetPair: AssetPair, matcherActor: ActorRef): Props =
    OrderBookActor.props(
      matcherActor,
      addressActors,
      orderBookSnapshotStore,
      assetPair,
      updateOrderBookCache(assetPair),
      marketStatuses.put(assetPair, _),
      newMatchingRules => rawMatchingRules.put(assetPair, newMatchingRules),
      normalize(assetPair),
      settings,
      transactionCreator.createTransaction,
      time,
      matchingRules(assetPair)
    )

  private val matcherQueue: MatcherQueue = settings.eventsQueue.tpe match {
    case "local" =>
      log.info("Events will be stored locally")
      new LocalMatcherQueue(settings.eventsQueue.local, new LocalQueueStore(db), time)

    case "kafka" =>
      log.info("Events will be stored in Kafka")
      new KafkaMatcherQueue(settings.eventsQueue.kafka)(materializer)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  private val getMarketStatus: AssetPair => Option[MarketStatus] = p => Option(marketStatuses.get(p))
  private val rateCache                                          = RateCache(db)

  private def validateOrder(o: Order): Either[MatcherError, Order] =
    for {
      _ <- OrderValidator.matcherSettingsAware(matcherPublicKey, blacklistedAddresses, blacklistedAssets, settings, rateCache)(o)
      _ <- OrderValidator.timeAware(time)(o)
      _ <- OrderValidator.marketAware(settings.orderFee, settings.deviation, getMarketStatus(o.assetPair), rateCache)(o)
      _ <- OrderValidator.blockchainAware(
        wavesBlockchainSyncClient,
        transactionCreator.createTransaction,
        matcherPublicKey.toAddress,
        time,
        settings.orderFee,
        settings.orderRestrictions,
        rateCache
      )(o)
      _ <- pairBuilder.validateAssetPair(o.assetPair)
    } yield o

  private implicit val errorContext: ErrorFormatterContext = (asset: Asset) => assetDecimalsCache.get(asset)

  lazy val matcherApiRoutes: Seq[ApiRoute] = {
    val keyHashStr = settings.restApi.apiKeyHash
    Seq(
      MatcherApiRoute(
        pairBuilder,
        matcherPublicKey,
        matcher,
        addressActors,
        balanceActor,
        matcherQueue.storeEvent,
        p => Option(orderBooks.get()).flatMap(_.get(p)),
        p => Option(marketStatuses.get(p)),
        assetPair => {
          lazy val default = RawMatchingRules(
            startOffset = 0,
            tickSize = denormalizeTickSize(assetPair, 1)
          )

          rawMatchingRules.computeIfAbsent(assetPair, _ => default).tickSize
        },
        validateOrder,
        orderBooksSnapshotCache,
        settings,
        () => status.get(),
        db,
        time,
        () => matcherQueue.lastProcessedOffset,
        () => matcherQueue.lastEventOffset,
        () => ExchangeTransactionCreator.minAccountFee(hasMatcherAccountScript),
        keyHashStr,
        rateCache,
        settings.allowedOrderVersions.filter(OrderValidator.checkOrderVersion(_, wavesBlockchainSyncClient).isRight)
      ),
      MatcherApiRouteV1(
        pairBuilder,
        orderBooksSnapshotCache,
        () => status.get(),
        keyHashStr,
        settings
      )
    )
  }

  lazy val matcherApiTypes: Set[Class[_]] =
    Set(
      classOf[MatcherApiRoute],
      classOf[MatcherApiRouteV1],
    )

  private val snapshotsRestore = Promise[Unit]()

  private lazy val assetPairsDb = AssetPairsDB(db)

  private lazy val orderBookSnapshotDB = OrderBookSnapshotDB(db)

  lazy val balanceActor: ActorRef = actorSystem.actorOf(
    BalanceActor.props(Function.untupled(balancesCache.get)),
    "balance"
  )

  lazy val storeActor: ActorRef = actorSystem.actorOf(
    StoreActor.props(matcherQueue.storeEvent),
    "store"
  )

  lazy val orderBookSnapshotStore: ActorRef = actorSystem.actorOf(
    OrderBookSnapshotStoreActor.props(orderBookSnapshotDB),
    "order-book-snapshot-store"
  )

  private val pongTimeout = new Timeout(settings.processConsumedTimeout * 2)

  lazy val matcher: ActorRef = actorSystem.actorOf(
    MatcherActor.props(
      settings,
      assetPairsDb, {
        case Left(msg) =>
          log.error(s"Can't start matcher: $msg")
          forceStopApplication(ErrorStartingMatcher)

        case Right((self, processedOffset)) =>
          snapshotsRestore.trySuccess(())
          matcherQueue.startConsume(
            processedOffset + 1,
            xs => {
              if (xs.isEmpty) Future.successful(())
              else {
                val assetPairs: Set[AssetPair] = xs.map { eventWithMeta =>
                  log.debug(s"Consumed $eventWithMeta")

                  self ! eventWithMeta
                  eventWithMeta.event.assetPair
                }(collection.breakOut)

                self
                  .ask(MatcherActor.PingAll(assetPairs))(pongTimeout)
                  .recover {
                    case NonFatal(e) => log.error("PingAll is timed out!", e)
                  }
                  .map(_ => ())
              }
            }
          )
      },
      orderBooks,
      orderBookProps,
      wavesBlockchainSyncClient.assetDescription
    ),
    MatcherActor.name
  )

  private lazy val orderDb = OrderDB(settings, db)

  private lazy val historyRouter = settings.orderHistory.map { orderHistorySettings =>
    actorSystem.actorOf(HistoryRouter.props(assetDecimalsCache.get, settings.postgresConnection, orderHistorySettings), "history-router")
  }

  private lazy val addressActors =
    actorSystem.actorOf(
      Props(
        new AddressDirectory(
          wavesBlockchainAsyncClient.unsafeTap { _.requestBalanceChanges() } |> { _.spendableBalanceChanges },
          settings,
          (address, startSchedules) =>
            Props(
              new AddressActor(
                address,
                balanceActor,
                storeActor,
                time,
                orderDb,
                wavesBlockchainSyncClient.forgedOrder,
                orderBookCache.get,
                startSchedules
              )
          ),
          historyRouter
        )
      ),
      "addresses"
    )

  private lazy val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private lazy val db = openDB(settings.dataDir)

  @volatile var matcherServerBinding: ServerBinding = _

  override def shutdown(): Future[Unit] = Future {
    log.info("Shutting down matcher")
    setStatus(Status.Stopping)

    Await.result(matcherServerBinding.unbind(), 10.seconds)

    val stopMatcherTimeout = 5.minutes
    matcherQueue.close(stopMatcherTimeout)

    orderBooksSnapshotCache.close()
    Await.result(gracefulStop(matcher, stopMatcherTimeout, MatcherActor.Shutdown), stopMatcherTimeout)
    materializer.shutdown()
    log.debug("Matcher's actor system has been shut down")
    db.close()
    log.debug("Matcher's database closed")
    log.info("Matcher shutdown successful")
  }

  private def checkDirectory(directory: File): Unit = if (!directory.exists()) {
    log.error(s"Failed to create directory '${directory.getPath}'")
    sys.exit(1)
  }

  override def start(): Unit = {

    val journalDir  = new File(settings.journalDataDir)
    val snapshotDir = new File(settings.snapshotsDataDir)

    journalDir.mkdirs()
    snapshotDir.mkdirs()

    checkDirectory(journalDir)
    checkDirectory(snapshotDir)

    log.info(s"Starting matcher on: ${settings.restApi.address}:${settings.restApi.port} ...")

    val combinedRoute = new CompositeHttpService(matcherApiTypes, matcherApiRoutes, settings.restApi).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, settings.restApi.address, settings.restApi.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress}")
    actorSystem.actorOf(
      ExchangeTransactionBroadcastActor
        .props(
          settings.exchangeTransactionBroadcast,
          time,
          wavesBlockchainSyncClient.wasForged,
          wavesBlockchainSyncClient.broadcastTx
        ),
      "exchange-transaction-broadcast"
    )

    actorSystem.actorOf(MatcherTransactionWriter.props(db, settings), MatcherTransactionWriter.name)

    val startGuard = for {
      _ <- waitSnapshotsRestored(settings.snapshotsLoadingTimeout)
      deadline = settings.startEventsProcessingTimeout.fromNow
      lastOffsetQueue <- getLastOffset(deadline)
      _ = log.info(s"Last queue offset is $lastOffsetQueue")
      _ <- waitOffsetReached(lastOffsetQueue, deadline)
      _ = log.info("Last offset has been reached, notify addresses")
    } yield addressActors ! AddressDirectory.StartSchedules

    startGuard.onComplete {
      case Success(_) => setStatus(Status.Working)
      case Failure(e) =>
        log.error(s"Can't start matcher: ${e.getMessage}", e)
        forceStopApplication(ErrorStartingMatcher)
    }
  }

  private def setStatus(newStatus: Status): Unit = {
    status.set(newStatus)
    log.info(s"Status now is $newStatus")
  }

  private def waitSnapshotsRestored(timeout: FiniteDuration): Future[Unit] = {
    val failure = Promise[Unit]()
    actorSystem.scheduler.scheduleOnce(timeout) {
      failure.failure(new TimeoutException(s"Can't restore snapshots in ${timeout.toSeconds} seconds"))
    }

    Future.firstCompletedOf[Unit](List(snapshotsRestore.future, failure.future))
  }

  private def getLastOffset(deadline: Deadline): Future[QueueEventWithMeta.Offset] = matcherQueue.lastEventOffset.recoverWith {
    case _: AskTimeoutException =>
      if (deadline.isOverdue()) Future.failed(new TimeoutException("Can't get last offset from queue"))
      else getLastOffset(deadline)
  }

  private def waitOffsetReached(lastQueueOffset: QueueEventWithMeta.Offset, deadline: Deadline): Future[Unit] = {
    def loop(p: Promise[Unit]): Unit = {
      val currentOffset = matcherQueue.lastProcessedOffset
      log.trace(s"offsets: $currentOffset >= $lastQueueOffset, deadline: ${deadline.isOverdue()}")
      if (currentOffset >= lastQueueOffset) p.success(())
      else if (deadline.isOverdue())
        p.failure(new TimeoutException(s"Can't process all events in ${settings.startEventsProcessingTimeout.toMinutes} minutes"))
      else actorSystem.scheduler.scheduleOnce(5.second)(loop(p))
    }

    val p = Promise[Unit]()
    loop(p)
    p.future
  }
}

object Matcher extends ScorexLogging {

  type StoreEvent = QueueEvent => Future[Option[QueueEventWithMeta]]

  sealed trait Status

  object Status {
    case object Starting extends Status
    case object Working  extends Status
    case object Stopping extends Status
  }
}
