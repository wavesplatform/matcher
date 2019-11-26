package com.wavesplatform.dex

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.{ask, gracefulStop}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import cats.syntax.functor._
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.{ApiRoute, CompositeHttpService => _}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.database._
import com.wavesplatform.dex.api.http.CompositeHttpService
import com.wavesplatform.dex.api.{MatcherApiRoute, MatcherApiRouteV1, OrderBookSnapshotHttpCache}
import com.wavesplatform.dex.caches.{MatchingRulesCache, RateCache}
import com.wavesplatform.dex.db._
import com.wavesplatform.dex.effect._
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.grpc.integration.DEXClient
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
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
import mouse.any.anySyntaxMouse
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Matcher(settings: MatcherSettings, gRPCExtensionClient: DEXClient)(implicit val actorSystem: ActorSystem, val materializer: ActorMaterializer)
    extends Extension
    with ScorexLogging {

  import actorSystem.dispatcher
  import com.wavesplatform.dex.Matcher._
  import gRPCExtensionClient.{grpcExecutionContext, wavesBlockchainAsyncClient}

  private val time = new NTP(settings.ntpServer)

  private val matcherKeyPair = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet().unsafeTap { x =>
    log.info(s"The DEX's public key: ${Base58.encode(x.publicKey.arr)}, account address: ${x.publicKey.toAddress.stringRepr}")
  }

  private def matcherPublicKey: PublicKey = matcherKeyPair

  @volatile private var lastProcessedOffset   = -1L
  private val status: AtomicReference[Status] = new AtomicReference(Status.Starting)

  private lazy val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private val pairBuilder = new AssetPairBuilder(settings,
                                                 getDescription(assetsDB, wavesBlockchainAsyncClient.assetDescription)(_),
                                                 settings.blacklistedAssets)(grpcExecutionContext)

  private val orderBooks = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])

  private var hasMatcherAccountScript: Boolean = false

  private lazy val assetsDB: AssetsDB = AssetsDB(db)

  private lazy val transactionCreator = new ExchangeTransactionCreator(matcherKeyPair, settings, hasMatcherAccountScript, assetsDB.unsafeGetHasScript)

  private implicit val errorContext: ErrorFormatterContext = {
    case Asset.Waves        => 8
    case asset: IssuedAsset => assetsDB.unsafeGetDecimals(asset)
  }

  private val orderBookCache     = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot](1000, 0.9f, 10)
  private val matchingRulesCache = new MatchingRulesCache(settings)
  private val rateCache          = RateCache(db)

  private val orderBooksSnapshotCache =
    new OrderBookSnapshotHttpCache(
      settings.orderBookSnapshotHttpCache,
      time,
      assetsDB.get(_).map(_.decimals),
      p => Option { orderBookCache.get(p) }
    )

  private val marketStatuses                                     = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)
  private val getMarketStatus: AssetPair => Option[MarketStatus] = p => Option(marketStatuses.get(p))

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook.AggregatedSnapshot): Unit = {
    orderBookCache.put(assetPair, newSnapshot)
    orderBooksSnapshotCache.invalidate(assetPair)
  }

  private def orderBookProps(assetPair: AssetPair, matcherActor: ActorRef, assetDecimals: Asset => Int): Props = {
    matchingRulesCache.setCurrentMatchingRuleForNewOrderBook(assetPair, lastProcessedOffset, assetDecimals)
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
      new KafkaMatcherQueue(settings.eventsQueue.kafka)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  private def validateOrder(o: Order): FutureResult[Order] = {

    import OrderValidator._

    /** Does not need additional access to the blockchain via gRPC */
    def syncValidation(orderAssetsDecimals: Asset => Int): Either[MatcherError, Order] = {

      lazy val actualTickSize = matchingRulesCache
        .getNormalizedRuleForNextOrder(o.assetPair, lastProcessedOffset, orderAssetsDecimals)
        .tickSize

      for {
        _ <- matcherSettingsAware(matcherPublicKey, blacklistedAddresses, settings, orderAssetsDecimals, rateCache)(o)
        _ <- timeAware(time)(o)
        _ <- marketAware(settings.orderFee, settings.deviation, getMarketStatus(o.assetPair))(o)
        _ <- tickSizeAware(actualTickSize)(o)
      } yield o
    }

    /** Needs additional asynchronous access to the blockchain */
    def asyncValidation(orderAssetsDescriptions: Asset => BriefAssetDescription)(implicit efc: ErrorFormatterContext): FutureResult[Order] = {
      blockchainAware(
        wavesBlockchainAsyncClient,
        transactionCreator.createTransaction,
        matcherPublicKey.toAddress,
        time,
        settings.orderFee,
        settings.orderRestrictions,
        orderAssetsDescriptions,
        rateCache,
        hasMatcherAccountScript
      )(o)(grpcExecutionContext, efc)
    }

    for {
      _ <- liftAsync { syncValidation(assetsDB.unsafeGetDecimals) }
      _ <- asyncValidation(assetsDB.unsafeGet)
    } yield o
  }

  private lazy val matcherApiRoutes: Seq[ApiRoute] = {
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
          matchingRulesCache.getDenormalizedRuleForNextOrder(assetPair, lastProcessedOffset, assetsDB.unsafeGetDecimals).tickSize
        },
        validateOrder,
        orderBooksSnapshotCache,
        settings,
        () => status.get(),
        db,
        time,
        () => lastProcessedOffset,
        () => matcherQueue.lastEventOffset,
        ExchangeTransactionCreator.getAdditionalFeeForScript(hasMatcherAccountScript),
        keyHashStr,
        rateCache,
        Future
          .sequence {
            settings.allowedOrderVersions.map(version =>
              OrderValidator.checkOrderVersion(version, wavesBlockchainAsyncClient.isFeatureActivated).value)
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

  private lazy val matcherActor: ActorRef =
    actorSystem.actorOf(
      MatcherActor.props(
        settings,
        assetPairsDB, {
          case Left(msg) =>
            log.error(s"Can't start MatcherActor: $msg")
            forceStopApplication(ErrorStartingMatcher)

          case Right((self, processedOffset)) =>
            snapshotsRestore.trySuccess(())
            matcherQueue.startConsume(
              processedOffset + 1,
              xs =>
                if (xs.isEmpty) Future.successful(Unit)
                else {

                  val eventAssets = xs.flatMap(_.event.assets)
                  val loadAssets  = Future.traverse(eventAssets)(getDescription(assetsDB, wavesBlockchainAsyncClient.assetDescription)(_).value)

                  loadAssets.flatMap { _ =>
                    val assetPairs: Set[AssetPair] = xs.map { eventWithMeta =>
                      log.debug(s"Consumed $eventWithMeta")
                      self ! eventWithMeta
                      lastProcessedOffset = eventWithMeta.offset
                      eventWithMeta.event.assetPair
                    }(collection.breakOut)

                    self
                      .ask(MatcherActor.PingAll(assetPairs))(pongTimeout)
                      .recover { case NonFatal(e) => log.error("PingAll is timed out!", e) }
                      .map(_ => ())

                  } andThen {
                    case Failure(ex) =>
                      log.error("Error while event processing occurred: ", ex)
                      forceStopApplication(ErrorStartingMatcher)
                    case _ =>
                  }
              }
            )
        },
        orderBooks,
        (assetPair, matcherActor) => orderBookProps(assetPair, matcherActor, assetsDB.unsafeGetDecimals),
        assetsDB.get(_)
      )(grpcExecutionContext),
      MatcherActor.name
    )

  private lazy val historyRouter = settings.orderHistory.map { orderHistorySettings =>
    actorSystem.actorOf(HistoryRouter.props(assetsDB.unsafeGetDecimals, settings.postgresConnection, orderHistorySettings), "history-router")
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
                settings.actorResponseTimeout,
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

  override def start(): Unit = {
    log.info(s"Starting matcher on: ${settings.restApi.address}:${settings.restApi.port} ...")

    def loadAllKnownAssets(): Future[Unit] = {
      val assetsToLoad = assetPairsDB.all().flatMap(_.assets) ++ settings.mentionedAssets

      Future
        .traverse(assetsToLoad) { asset =>
          getDecimals(assetsDB, wavesBlockchainAsyncClient.assetDescription)(asset).value.tupleLeft(asset)
        }
        .map { xs =>
          val notFoundAssets = xs.collect { case (id, Left(_)) => id }
          if (notFoundAssets.isEmpty) Unit
          else {
            log.error(s"Can't load assets: ${notFoundAssets.mkString(", ")}. Try to sync up your node with the network.")
            forceStopApplication(ErrorStartingMatcher)
          }
        }
    }

    val startGuard = for {
      _                <- loadAllKnownAssets()
      hasMatcherScript <- wavesBlockchainAsyncClient.hasScript(matcherKeyPair)
    } yield {

      hasMatcherAccountScript = hasMatcherScript
      val combinedRoute = new CompositeHttpService(matcherApiTypes, matcherApiRoutes, settings.restApi).compositeRoute
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
    case e: TimeoutException =>
      log.warn(s"During receiving last offset", e)
      if (deadline.isOverdue()) Future.failed(new RuntimeException("Can't get the last offset from queue", e))
      else getLastOffset(deadline)

    case e: Throwable =>
      log.error(s"Can't catch ${e.getClass.getName}", e)
      throw e
  }

  private def waitOffsetReached(lastQueueOffset: QueueEventWithMeta.Offset, deadline: Deadline): Future[Unit] = {
    def loop(p: Promise[Unit]): Unit = {
      log.trace(s"offsets: $lastProcessedOffset >= $lastQueueOffset, deadline: ${deadline.isOverdue()}")
      if (lastProcessedOffset >= lastQueueOffset) p.success(())
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

  private def getDecimals(assetsDB: AssetsDB, assetDesc: IssuedAsset => Future[Option[BriefAssetDescription]])(asset: Asset)(
      implicit ec: ExecutionContext): FutureResult[(Asset, Int)] =
    getDescription(assetsDB, assetDesc)(asset).map(x => asset -> x.decimals)(catsStdInstancesForFuture)

  private def getDescription(assetsDB: AssetsDB, assetDesc: IssuedAsset => Future[Option[BriefAssetDescription]])(asset: Asset)(
      implicit ec: ExecutionContext): FutureResult[BriefAssetDescription] =
    asset match {
      case Waves => AssetsDB.wavesLifted
      case asset: IssuedAsset =>
        assetsDB.get(asset) match {
          case Some(x) => liftValueAsync[BriefAssetDescription](x)
          case None =>
            EitherT {
              assetDesc(asset)
                .map {
                  _.toRight[MatcherError](error.AssetNotFound(asset))
                    .map { desc =>
                      BriefAssetDescription(desc.name, desc.decimals, desc.hasScript) unsafeTap { assetsDB.put(asset, _) }
                    }
                }
            }
        }
    }
}
