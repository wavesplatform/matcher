package com.wavesplatform.dex

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.{HttpServerTerminated, HttpTerminated, ServerBinding}
import akka.pattern.{ask, gracefulStop}
import akka.stream.Materializer
import akka.util.Timeout
import cats.data.EitherT
import cats.instances.future._
import cats.syntax.functor._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.orderbook.OrderBookActor.MarketStatus
import com.wavesplatform.dex.actors.orderbook.{AggregatedOrderBookActor, OrderBookActor, OrderBookSnapshotStoreActor}
import com.wavesplatform.dex.actors.tx.{BroadcastExchangeTransactionActor, CreateExchangeTransactionActor, WriteExchangeTransactionActor}
import com.wavesplatform.dex.actors.{MatcherActor, OrderBookAskAdapter, SpendableBalancesActor}
import com.wavesplatform.dex.api.http.routes.{MatcherApiRoute, MatcherApiRouteV1}
import com.wavesplatform.dex.api.http.{CompositeHttpService, OrderBookHttpInfo}
import com.wavesplatform.dex.api.routes.ApiRoute
import com.wavesplatform.dex.api.ws.routes._
import com.wavesplatform.dex.caches.{MatchingRulesCache, OrderFeeSettingsCache, RateCache}
import com.wavesplatform.dex.db._
import com.wavesplatform.dex.db.leveldb._
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.{EitherExt2, ScorexLogging}
import com.wavesplatform.dex.effect._
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.grpc.integration.WavesBlockchainClientBuilder
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.BalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.history.HistoryRouter
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue._
import com.wavesplatform.dex.settings.{MatcherSettings, OrderFeeSettings}
import com.wavesplatform.dex.time.NTP
import monix.eval.Task
import mouse.any.anySyntaxMouse

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

// TODO Remove start, merge with Application
class Matcher(settings: MatcherSettings)(implicit val actorSystem: ActorSystem) extends ScorexLogging {

  import com.wavesplatform.dex.Matcher._

  private implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  private val monixScheduler                              = monix.execution.Scheduler.Implicits.global
  private val grpcExecutionContext                        = actorSystem.dispatchers.lookup("akka.actor.grpc-dispatcher")

  private val time = new NTP(settings.ntpServer)

  private val wavesBlockchainAsyncClient =
    WavesBlockchainClientBuilder.async(
      settings.wavesBlockchainClient,
      monixScheduler,
      grpcExecutionContext
    )

  private implicit val materializer: Materializer = Materializer.matFromSystem(actorSystem)

  private val matcherKeyPair = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet().unsafeTap { x =>
    log.info(s"The DEX's public key: ${Base58.encode(x.publicKey.arr)}, account address: ${x.publicKey.toAddress.stringRepr}")
  }

  private def matcherPublicKey: PublicKey   = matcherKeyPair
  @volatile private var lastProcessedOffset = -1L

  private val status: AtomicReference[Status] = new AtomicReference(Status.Starting)

  private lazy val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private val pairBuilder = {
    new AssetPairBuilder(settings, getDescription(assetsCache, wavesBlockchainAsyncClient.assetDescription)(_), settings.blacklistedAssets)
  }

  private var hasMatcherAccountScript: Boolean = false
  private lazy val assetsCache                 = AssetsStorage.cache { AssetsStorage.levelDB(db) }

  private val orderBooks          = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val orderBookAskAdapter = new OrderBookAskAdapter(orderBooks, settings.actorResponseTimeout)
  private val orderBookHttpInfo =
    new OrderBookHttpInfo(settings.orderBookSnapshotHttpCache, orderBookAskAdapter, time, assetsCache.get(_).map(_.decimals))

  private lazy val transactionCreator = new ExchangeTransactionCreator(
    matcherKeyPair,
    settings.exchangeTxBaseFee,
    hasMatcherAccountScript,
    assetsCache.unsafeGetHasScript
  )

  private implicit val errorContext: ErrorFormatterContext = _.fold(8)(assetsCache.unsafeGetDecimals)

  private val matchingRulesCache    = new MatchingRulesCache(settings)
  private val orderFeeSettingsCache = new OrderFeeSettingsCache(settings.orderFee)
  private val rateCache             = RateCache(db)

  private def getDecimalsFromCache(asset: Asset): FutureResult[Int] = getDecimals(assetsCache, wavesBlockchainAsyncClient.assetDescription)(asset)

  private def orderBookProps(assetPair: AssetPair, matcherActor: ActorRef, assetDecimals: Asset => Int): Props = {
    matchingRulesCache.setCurrentMatchingRuleForNewOrderBook(assetPair, lastProcessedOffset, assetDecimals)
    OrderBookActor.props(
      OrderBookActor.Settings(AggregatedOrderBookActor.Settings(settings.webSocketSettings.messagesInterval)),
      matcherActor,
      addressActors,
      orderBookSnapshotStore,
      assetPair,
      amountDecimals = assetDecimals(assetPair.amountAsset),
      priceDecimals = assetDecimals(assetPair.priceAsset),
      time,
      matchingRules = matchingRulesCache.getMatchingRules(assetPair, assetDecimals),
      updateCurrentMatchingRules = actualMatchingRule => matchingRulesCache.updateCurrentMatchingRule(assetPair, actualMatchingRule),
      normalizeMatchingRule = denormalizedMatchingRule => denormalizedMatchingRule.normalize(assetPair, assetDecimals),
      Fee.getMakerTakerFeeByOffset(orderFeeSettingsCache),
      settings.orderRestrictions.get(assetPair)
    )
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

    def actualOrderFeeSettings: OrderFeeSettings = orderFeeSettingsCache.getSettingsForOffset(lastProcessedOffset + 1)

    /** Does not need additional access to the blockchain via gRPC */
    def syncValidation(marketStatus: Option[MarketStatus], orderAssetsDecimals: Asset => Int): Either[MatcherError, Order] = {

      lazy val actualTickSize = matchingRulesCache
        .getNormalizedRuleForNextOrder(o.assetPair, lastProcessedOffset, orderAssetsDecimals)
        .tickSize

      for {
        _ <- matcherSettingsAware(matcherPublicKey, blacklistedAddresses, settings, orderAssetsDecimals, rateCache, actualOrderFeeSettings)(o)
        _ <- timeAware(time)(o)
        _ <- tickSizeAware(actualTickSize)(o)
        _ <- if (settings.deviation.enabled) marketAware(actualOrderFeeSettings, settings.deviation, marketStatus)(o) else success
      } yield o
    }

    /** Needs additional asynchronous access to the blockchain */
    def asyncValidation(orderAssetsDescriptions: Asset => BriefAssetDescription): FutureResult[Order] =
      blockchainAware(
        wavesBlockchainAsyncClient,
        transactionCreator.createTransaction,
        matcherPublicKey.toAddress,
        time,
        actualOrderFeeSettings,
        settings.orderRestrictions.get(o.assetPair),
        orderAssetsDescriptions,
        rateCache,
        hasMatcherAccountScript
      )(o)

    for {
      marketStatus <- {
        if (settings.deviation.enabled) EitherT(orderBookAskAdapter.getMarketStatus(o.assetPair))
        else liftValueAsync(Option.empty[OrderBookActor.MarketStatus])
      }
      _ <- liftAsync { syncValidation(marketStatus, assetsCache.unsafeGetDecimals) }
      _ <- asyncValidation(assetsCache.unsafeGet)
    } yield o
  }

  private val maybeApiKeyHash: Option[Array[Byte]] = Option(settings.restApi.apiKeyHash) filter (_.nonEmpty) map Base58.decode

  private lazy val httpApiRouteV0: MatcherApiRoute =
    MatcherApiRoute(
      pairBuilder,
      matcherPublicKey,
      matcherActor,
      addressActors,
      matcherQueue.storeEvent,
      p => Option { orderBooks.get() } flatMap (_ get p),
      orderBookHttpInfo,
      getActualTickSize = assetPair => {
        matchingRulesCache.getDenormalizedRuleForNextOrder(assetPair, lastProcessedOffset, assetsCache.unsafeGetDecimals).tickSize
      },
      validateOrder,
      settings,
      () => status.get(),
      orderDB,
      time,
      () => lastProcessedOffset,
      () => matcherQueue.lastEventOffset,
      ExchangeTransactionCreator.getAdditionalFeeForScript(hasMatcherAccountScript),
      maybeApiKeyHash,
      rateCache,
      validatedAllowedOrderVersions = () => {
        Future
          .sequence {
            settings.allowedOrderVersions.map(version =>
              OrderValidator.checkOrderVersion(version, wavesBlockchainAsyncClient.isFeatureActivated).value)
          }
          .map { _.collect { case Right(version) => version } }
      },
      () => orderFeeSettingsCache.getSettingsForOffset(lastProcessedOffset + 1)
    )

  private lazy val httpApiRouteV1 = MatcherApiRouteV1(pairBuilder, orderBookHttpInfo, () => status.get(), maybeApiKeyHash)

  private lazy val wsApiRoute = MatcherWebSocketRoute(addressActors,
                                                      matcherActor,
                                                      time,
                                                      pairBuilder,
                                                      p => Option { orderBooks.get() } flatMap (_ get p),
                                                      maybeApiKeyHash,
                                                      settings.webSocketSettings)

  private lazy val matcherApiRoutes: Seq[ApiRoute] = Seq(httpApiRouteV0, httpApiRouteV1, wsApiRoute)

  lazy val matcherApiTypes: Set[Class[_]] = Set(
    classOf[MatcherApiRoute],
    classOf[MatcherApiRouteV1],
    classOf[MatcherWebSocketRoute]
  )

  private val snapshotsRestore = Promise[Unit]()

  private lazy val db                  = openDB(settings.dataDir)
  private lazy val assetPairsDB        = AssetPairsDB(db)
  private lazy val orderBookSnapshotDB = OrderBookSnapshotDB(db)
  private lazy val orderDB             = OrderDB(settings.orderDb, db)

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
            forceStopApplication(RecoveryError)

          case Right((self, processedOffset)) =>
            snapshotsRestore.trySuccess(())
            matcherQueue.startConsume(
              processedOffset + 1,
              xs =>
                if (xs.isEmpty) Future.successful(Unit)
                else {
                  val eventAssets = xs.flatMap(_.event.assets)
                  val loadAssets  = Future.traverse(eventAssets)(getDescription(assetsCache, wavesBlockchainAsyncClient.assetDescription)(_).value)

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
                      forceStopApplication(EventProcessingError)
                    case _ =>
                  }
              }
            )
        },
        orderBooks,
        (assetPair, matcherActor) => orderBookProps(assetPair, matcherActor, assetsCache.unsafeGetDecimals),
        assetsCache.get(_)
      ),
      MatcherActor.name
    )

  private lazy val historyRouter = settings.orderHistory.map { orderHistorySettings =>
    actorSystem.actorOf(HistoryRouter.props(assetsCache.unsafeGetDecimals, settings.postgresConnection, orderHistorySettings), "history-router")
  }

  private lazy val addressActors =
    actorSystem.actorOf(
      Props(
        new AddressDirectoryActor(
          orderDB,
          createAddressActor,
          historyRouter
        )
      ),
      "addresses"
    )

  private def sendBalanceChanges(bufferSize: Int)(recipient: ActorRef): Unit = {

    def aggregateChangesByAddress(xs: List[BalanceChanges]): Map[Address, Map[Asset, Long]] = xs.foldLeft(Map.empty[Address, Map[Asset, Long]]) {
      case (result, bc) => result.updated(bc.address, result.getOrElse(bc.address, Map.empty) + (bc.asset -> bc.balance))
    }

    wavesBlockchainAsyncClient.realTimeBalanceChanges
      .bufferIntrospective(bufferSize)
      .map(aggregateChangesByAddress)
      .mapEval { xs =>
        Task
          .traverse { xs.valuesIterator.flatMap(_.keysIterator).toList }(asset => Task fromFuture getDecimalsFromCache(asset).value)
          .map(_ => xs)
      }
      .foreach { recipient ! SpendableBalancesActor.Command.UpdateStates(_) }(monixScheduler)
  }

  private val spendableBalancesActor = {
    actorSystem.actorOf(
      Props(
        new SpendableBalancesActor(
          wavesBlockchainAsyncClient.spendableBalances,
          wavesBlockchainAsyncClient.allAssetsSpendableBalance,
          addressActors
        )
      )
    ) unsafeTap sendBalanceChanges(settings.wavesBlockchainClient.balanceStreamBufferSize)
  }

  private def validateForAddress(ao: AcceptedOrder, tradableBalance: Map[Asset, Long]): Future[Either[MatcherError, Unit]] = {
    for {
      hasOrderInBlockchain <- liftFutureAsync(wavesBlockchainAsyncClient.forgedOrder(ao.id))
      orderBookCache       <- EitherT(orderBookAskAdapter.getAggregatedSnapshot(ao.order.assetPair))
      _ <- EitherT.fromEither {
        OrderValidator
          .accountStateAware(
            ao.order.sender,
            tradableBalance.withDefaultValue(0L),
            hasOrderInBlockchain,
            orderBookCache.getOrElse(OrderBookAggregatedSnapshot.empty)
          )(ao)
      }
    } yield ()
  }.value

  private def createAddressActor(address: Address, startSchedules: Boolean): Props = {
    Props(
      new AddressActor(
        address,
        time,
        orderDB,
        validateForAddress,
        matcherQueue.storeEvent,
        startSchedules,
        spendableBalancesActor,
        settings.addressActorSettings
      )
    )
  }

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdown(): Future[Unit] = {

    setStatus(Status.Stopping)

    val r = for {
      _ <- { log.info("Shutting down all WebSocket connections..."); wsApiRoute.gracefulShutdown() }
      _ <- {
        log.info("Shutting down HTTP and WS servers...")
        Option(matcherServerBinding).fold[Future[HttpTerminated]](Future.successful(HttpServerTerminated))(_.terminate(1.second))
      }
      _ <- { log.info("Shutting down actors..."); gracefulStop(matcherActor, 3.seconds, MatcherActor.Shutdown) }
      _ <- { log.info("Shutting down gRPC client..."); wavesBlockchainAsyncClient.close() }
      _ <- { log.info("Shutting down queue..."); Future { blocking(matcherQueue close 5.seconds) } }
      _ <- { log.info("Shutting down materializer..."); Future.successful(materializer.shutdown()) }
      _ <- { log.info("Shutting down DB..."); Future { blocking(db.close()) } }
    } yield ()

    r.andThen {
      case Success(_) => log.info("Matcher stopped")
      case Failure(e) => log.error("Failed to stop matcher", e)
    }
  }

  def start(): Unit = {

    def loadAllKnownAssets(): Future[Unit] =
      Future(blocking(assetPairsDB.all()).flatMap(_.assets) ++ settings.mentionedAssets).flatMap { assetsToLoad =>
        Future
          .traverse(assetsToLoad)(asset => getDecimalsFromCache(asset).value tupleLeft asset)
          .map { xs =>
            val notFoundAssets = xs.collect { case (id, Left(_)) => id }
            if (notFoundAssets.nonEmpty) {
              log.error(s"Can't load assets: ${notFoundAssets.mkString(", ")}. Try to sync up your node with the network.")
              forceStopApplication(UnsynchronizedNodeError)
            }
          }
      }

    actorSystem.actorOf(
      CreateExchangeTransactionActor.props(transactionCreator.createTransaction),
      CreateExchangeTransactionActor.name
    )

    actorSystem.actorOf(WriteExchangeTransactionActor.props(db), WriteExchangeTransactionActor.name)

    actorSystem.actorOf(
      BroadcastExchangeTransactionActor
        .props(
          settings.exchangeTransactionBroadcast,
          time,
          wavesBlockchainAsyncClient.wereForged,
          wavesBlockchainAsyncClient.broadcastTx
        ),
      "exchange-transaction-broadcast"
    )

    val startGuard = for {
      (_, http) <- {
        log.info("Loading known assets ...")
        loadAllKnownAssets()
      } zip {
        log.info("Checking matcher's account script ...")
        wavesBlockchainAsyncClient.hasScript(matcherKeyPair).map(hasMatcherAccountScript = _)
      } zip {
        Future(
          blocking {
            log.info(s"Initializing HTTP ...")
            Http() // May take 3+ seconds
          }
        )
      }

      _ <- {
        log.info("Preparing HTTP service ...")
        // Indirectly initializes matcherActor, so it must be after loadAllKnownAssets
        val combinedRoute = new CompositeHttpService(matcherApiTypes, matcherApiRoutes, settings.restApi).compositeRoute

        log.info(s"Binding REST and WebSocket API ${settings.restApi.address}:${settings.restApi.port} ...")
        http.bindAndHandle(combinedRoute, settings.restApi.address, settings.restApi.port)
      } map { serverBinding =>
        matcherServerBinding = serverBinding
        log.info(s"REST and WebSocket API bound to ${matcherServerBinding.localAddress}")
      }

      deadline = settings.startEventsProcessingTimeout.fromNow
      (_, lastOffsetQueue) <- {
        log.info("Waiting all snapshots are restored ...")
        waitSnapshotsRestored(settings.snapshotsLoadingTimeout).map(_ => log.info("All snapshots are restored"))
      } zip {
        log.info("Getting last queue offset ...")
        getLastOffset(deadline)
      }

      _ <- {
        log.info(s"Last queue offset is $lastOffsetQueue")
        waitOffsetReached(lastOffsetQueue, deadline)
      }

      connectedNodeAddress <- wavesBlockchainAsyncClient.getNodeAddress
    } yield {
      log.info("Last offset has been reached, notify addresses")
      log.info(s"DEX server is connected to Node with an address: ${connectedNodeAddress.getHostAddress}")
      addressActors ! AddressDirectoryActor.StartSchedules
    }

    startGuard.onComplete {
      case Success(_) => setStatus(Status.Working)
      case Failure(e) =>
        log.error(s"Can't start matcher: ${e.getMessage}", e)
        forceStopApplication(StartingMatcherError)
    }
  }

  private def setStatus(newStatus: Status): Unit = {
    status.set(newStatus)
    log.info(s"Status now is $newStatus")
  }

  private def waitSnapshotsRestored(wait: FiniteDuration): Future[Unit] =
    Future.firstCompletedOf(List(snapshotsRestore.future, timeout(wait, "wait snapshots restored")))

  private def getLastOffset(deadline: Deadline): Future[QueueEventWithMeta.Offset] = matcherQueue.lastEventOffset.recoverWith {
    case e: TimeoutException =>
      log.warn(s"During receiving last offset", e)
      if (deadline.isOverdue()) Future.failed(new TimeoutException("Can't get the last offset from queue"))
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

  private def timeout(after: FiniteDuration, label: String): Future[Nothing] = {
    val failure = Promise[Nothing]()
    actorSystem.scheduler.scheduleOnce(after) {
      failure.failure(new TimeoutException(s"$after is out: $label"))
    }
    failure.future
  }
}

object Matcher extends ScorexLogging {

  def forceStopApplication(reason: ApplicationStopReason): Unit = System.exit(reason.code)

  type StoreEvent = QueueEvent => Future[Option[QueueEventWithMeta]]

  sealed trait Status

  object Status {
    case object Starting extends Status
    case object Working  extends Status
    case object Stopping extends Status
  }

  private def getDecimals(assetsCache: AssetsStorage, assetDesc: IssuedAsset => Future[Option[BriefAssetDescription]])(asset: Asset)(
      implicit ec: ExecutionContext): FutureResult[Int] =
    getDescription(assetsCache, assetDesc)(asset).map(_.decimals)(catsStdInstancesForFuture)

  private def getDescription(assetsCache: AssetsStorage, assetDesc: IssuedAsset => Future[Option[BriefAssetDescription]])(asset: Asset)(
      implicit ec: ExecutionContext): FutureResult[BriefAssetDescription] = asset match {
    case Waves => AssetsStorage.wavesLifted
    case asset: IssuedAsset =>
      assetsCache.get(asset) match {
        case Some(x) => liftValueAsync[BriefAssetDescription](x)
        case None =>
          EitherT {
            assetDesc(asset)
              .map {
                _.toRight[MatcherError](error.AssetNotFound(asset))
                  .map { desc =>
                    BriefAssetDescription(desc.name, desc.decimals, desc.hasScript) unsafeTap { assetsCache.put(asset, _) }
                  }
              }
          }
      }
  }
}
