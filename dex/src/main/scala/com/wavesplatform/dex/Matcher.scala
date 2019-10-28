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
import cats.implicits._
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.{ApiRoute, CompositeHttpService => _}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.database._
import com.wavesplatform.dex.Matcher.Status
import com.wavesplatform.dex.api.http.CompositeHttpService
import com.wavesplatform.dex.api.{MatcherApiRoute, MatcherApiRouteV1, OrderBookSnapshotHttpCache}
import com.wavesplatform.dex.caches.{AssetDecimalsCache, MatchingRulesCache, RateCache}
import com.wavesplatform.dex.db.{AccountStorage, AssetPairsDB, OrderBookSnapshotDB, OrderDB}
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.grpc.integration.DEXClient
import com.wavesplatform.dex.history.HistoryRouter
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.market._
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue._
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.extensions.Extension
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{ErrorStartingMatcher, NTP, ScorexLogging, forceStopApplication}
import monix.eval.Coeval
import mouse.any._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Matcher(settings: MatcherSettings, gRPCExtensionClient: DEXClient)(implicit val actorSystem: ActorSystem, val materializer: ActorMaterializer)
    extends Extension
    with ScorexLogging {

  import actorSystem.dispatcher
  import gRPCExtensionClient.{grpcExecutionContext, wavesBlockchainAsyncClient, wavesBlockchainSyncClient}

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

  private lazy val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private val pairBuilder = new AssetPairBuilder(settings, wavesBlockchainAsyncClient.assetDescription, blacklistedAssets)(grpcExecutionContext)
  private val orderBooks  = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])

  private def hasMatcherAccountScript: Future[Boolean] = wavesBlockchainAsyncClient.hasScript(matcherKeyPair)

  private val transactionCreator = new ExchangeTransactionCreator(matcherKeyPair,
                                                                  settings,
                                                                  hasMatcherAccountScript,
                                                                  wavesBlockchainAsyncClient.hasScript,
                                                                  wavesBlockchainAsyncClient.isFeatureActivated)

  private val assetDecimalsCache                           = new AssetDecimalsCache(wavesBlockchainSyncClient.assetDescription)
  private implicit val errorContext: ErrorFormatterContext = (asset: Asset) => assetDecimalsCache.get(asset)

  private val orderBookCache     = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot](1000, 0.9f, 10)
  private val matchingRulesCache = new MatchingRulesCache(settings)
  private val rateCache          = RateCache(db)

  private val orderBooksSnapshotCache =
    new OrderBookSnapshotHttpCache(
      settings.orderBookSnapshotHttpCache,
      time,
      wavesBlockchainAsyncClient.assetDecimals,
      p => Option { orderBookCache.get(p) }
    )(grpcExecutionContext)

  private val marketStatuses                                     = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)
  private val getMarketStatus: AssetPair => Option[MarketStatus] = p => Option(marketStatuses.get(p))

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook.AggregatedSnapshot): Unit = {
    orderBookCache.put(assetPair, newSnapshot)
    orderBooksSnapshotCache.invalidate(assetPair)
  }

  private def orderBookProps(assetPair: AssetPair, matcherActor: ActorRef, assetDecimals: Asset => Int): Props = {
    matchingRulesCache.setCurrentMatchingRuleForNewOrderBook(assetPair, matcherQueue.lastProcessedOffset, assetDecimals)
    OrderBookActor.props(
      matcherActor,
      addressActors,
      orderBookSnapshotStore,
      assetPair,
      updateOrderBookCache(assetPair),
      marketStatuses.put(assetPair, _),
      settings,
      transactionCreator.createTransaction,
      time,
      matchingRules = matchingRulesCache.getMatchingRules(assetPair, assetDecimals),
      updateCurrentMatchingRules = actualMatchingRule => matchingRulesCache.updateCurrentMatchingRule(assetPair, actualMatchingRule),
      normalizeMatchingRule = denormalizedMatchingRule => denormalizedMatchingRule.normalize(assetPair, assetDecimals),
    )(grpcExecutionContext)
  }

  private val matcherQueue: MatcherQueue = settings.eventsQueue.tpe match {
    case "local" =>
      log.info("Events will be stored locally")
      new LocalMatcherQueue(settings.eventsQueue.local, new LocalQueueStore(db), time)

    case "kafka" =>
      log.info("Events will be stored in Kafka")
      new KafkaMatcherQueue(settings.eventsQueue.kafka)(materializer)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  private def validateOrder(o: Order): OrderValidator.FutureResult[Order] = {

    import OrderValidator._

    /** Does not need additional access to the blockchain via gRPC */
    def syncValidation(orderAssetsDecimals: Asset => Int): Either[MatcherError, Order] = {

      lazy val actualTickSize = matchingRulesCache
        .getNormalizedRuleForNextOrder(o.assetPair, matcherQueue.lastProcessedOffset, orderAssetsDecimals)
        .tickSize

      for {
        _ <- matcherSettingsAware(matcherPublicKey, blacklistedAddresses, blacklistedAssets, settings, orderAssetsDecimals, rateCache)(o)
        _ <- timeAware(time)(o)
        _ <- marketAware(settings.orderFee, settings.deviation, getMarketStatus(o.assetPair))(o)
        _ <- tickSizeAware(actualTickSize)(o)
      } yield o
    }

    /** Needs additional asynchronous access to the blockchain */
    def asyncValidation(orderAssetsDecimals: Asset => Int): OrderValidator.FutureResult[Order] = {
      blockchainAware(
        wavesBlockchainAsyncClient,
        transactionCreator.createTransaction,
        matcherPublicKey.toAddress,
        time,
        settings.orderFee,
        settings.orderRestrictions,
        orderAssetsDecimals,
        rateCache
      )(o)(grpcExecutionContext)
    }

    for {
      assetDecimals <- assetDecimalsAware(wavesBlockchainAsyncClient.assetDecimals)(o)
      _             <- liftAsync { syncValidation(assetDecimals) }
      _             <- asyncValidation(assetDecimals)
    } yield o
  }

  def createMatcherApiRoutes(matchingRulesAssetDecimals: Asset => Int): Coeval[Seq[ApiRoute]] = Coeval.evalOnce {
    matcherActor = createMatcherActor(matchingRulesAssetDecimals).value
    val keyHashStr = settings.restApi.apiKeyHash
    Seq(
      MatcherApiRoute(
        pairBuilder,
        matcherPublicKey,
        matcherActor,
        addressActors,
        matcherQueue.storeEvent,
        p => Option { orderBooks.get() } flatMap (_ get p),
        p => Option { marketStatuses.get(p) },
        getActualTickSize = assetPair => {
          matchingRulesCache.getDenormalizedRuleForNextOrder(assetPair, matcherQueue.lastProcessedOffset, matchingRulesAssetDecimals).tickSize
        },
        validateOrder,
        orderBooksSnapshotCache,
        settings,
        () => status.get(),
        db,
        time,
        () => matcherQueue.lastProcessedOffset,
        () => matcherQueue.lastEventOffset,
        () => hasMatcherAccountScript.map { ExchangeTransactionCreator.getAdditionalFee },
        keyHashStr,
        rateCache,
        Future
          .sequence {
            settings.allowedOrderVersions.map(version => OrderValidator.checkOrderVersion(version, wavesBlockchainAsyncClient).value)
          }
          .map { _.collect { case Right(version) => version } }
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

  lazy val matcherApiTypes: Set[Class[_]] = Set(
    classOf[MatcherApiRoute],
    classOf[MatcherApiRouteV1]
  )

  private val snapshotsRestore = Promise[Unit]()

  private lazy val db                  = openDB(settings.dataDir)
  private lazy val assetPairsDB        = AssetPairsDB(db)
  private lazy val orderBookSnapshotDB = OrderBookSnapshotDB(db)
  private lazy val orderDB             = OrderDB(settings, db)

  lazy val orderBookSnapshotStore: ActorRef = actorSystem.actorOf(
    OrderBookSnapshotStoreActor.props(orderBookSnapshotDB),
    "order-book-snapshot-store"
  )

  private val pongTimeout = new Timeout(settings.processConsumedTimeout * 2)

  def createMatcherActor(matchingRulesAssetDecimals: Asset => Int): Coeval[ActorRef] = Coeval.evalOnce {
    actorSystem.actorOf(
      MatcherActor.props(
        settings,
        assetPairsDB, {
          case Left(msg) =>
            log.error(s"Can't start matcher: $msg")
            forceStopApplication(ErrorStartingMatcher)

          case Right((self, processedOffset)) =>
            snapshotsRestore.trySuccess(())
            matcherQueue.startConsume(
              processedOffset + 1,
              xs => {
                if (xs.isEmpty) Future.successful(Unit)
                else {

                  val assetPairs: Set[AssetPair] = xs.map { eventWithMeta =>
                    log.debug(s"Consumed $eventWithMeta")
                    self ! eventWithMeta
                    eventWithMeta.event.assetPair
                  }(collection.breakOut)

                  self
                    .ask(MatcherActor.PingAll(assetPairs))(pongTimeout)
                    .recover { case NonFatal(e) => log.error("PingAll is timed out!", e) }
                    .map(_ => Unit)
                }
              }
            )
        },
        orderBooks,
        (assetPair, matcherActor) => orderBookProps(assetPair, matcherActor, matchingRulesAssetDecimals),
        wavesBlockchainAsyncClient.assetDescription
      )(grpcExecutionContext),
      MatcherActor.name
    )
  }

  private lazy val historyRouter = settings.orderHistory.map { orderHistorySettings =>
    actorSystem.actorOf(HistoryRouter.props(assetDecimalsCache.get, settings.postgresConnection, orderHistorySettings), "history-router")
  }

  private lazy val addressActors =
    actorSystem.actorOf(
      Props(
        new AddressDirectory(
          wavesBlockchainAsyncClient.unsafeTap { _.requestBalanceChanges() } |> { _.spendableBalanceChanges },
          settings,
          orderDB,
          (address, startSchedules) =>
            Props(
              new AddressActor(
                address,
                asset => wavesBlockchainAsyncClient.spendableBalance(address, asset),
                time,
                orderDB,
                wavesBlockchainAsyncClient.forgedOrder,
                matcherQueue.storeEvent,
                orderBookCache.getOrDefault(_, OrderBook.AggregatedSnapshot()),
                startSchedules,
                settings.actorResponseTimeout - settings.actorResponseTimeout / 10 // Should be enough
              )
          ),
          historyRouter
        )
      ),
      "addresses"
    )

  @volatile var matcherServerBinding: ServerBinding = _
  var matcherActor: ActorRef                        = _

  override def shutdown(): Future[Unit] = Future {
    log.info("Shutting down matcher")
    setStatus(Status.Stopping)

    Await.result(matcherServerBinding.unbind(), 10.seconds)

    val stopMatcherTimeout = 5.minutes
    matcherQueue.close(stopMatcherTimeout)

    orderBooksSnapshotCache.close()
    Await.result(gracefulStop(matcherActor, stopMatcherTimeout, MatcherActor.Shutdown), stopMatcherTimeout)
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

    val startGuard = {
      wavesBlockchainAsyncClient.getAssetDecimalsForMatchingRules(settings.matchingRules.keySet) flatMap { assetDecimals =>
        val combinedRoute = new CompositeHttpService(matcherApiTypes, createMatcherApiRoutes(assetDecimals).value, settings.restApi).compositeRoute
        matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, settings.restApi.address, settings.restApi.port), 5.seconds)

        log.info(s"Matcher bound to ${matcherServerBinding.localAddress}")
        actorSystem.actorOf(
          ExchangeTransactionBroadcastActor
            .props(
              settings.exchangeTransactionBroadcast,
              time,
              wavesBlockchainAsyncClient.wasForged,
              wavesBlockchainAsyncClient.broadcastTx
            ),
          "exchange-transaction-broadcast"
        )

        actorSystem.actorOf(MatcherTransactionWriter.props(db, settings), MatcherTransactionWriter.name)

        for {
          _ <- waitSnapshotsRestored(settings.snapshotsLoadingTimeout)
          deadline = settings.startEventsProcessingTimeout.fromNow
          lastOffsetQueue <- getLastOffset(deadline)
          _ = log.info(s"Last queue offset is $lastOffsetQueue")
          _ <- waitOffsetReached(lastOffsetQueue, deadline)
          _ = log.info("Last offset has been reached, notify addresses")
        } yield addressActors ! AddressDirectory.StartSchedules
      }
    }

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
