package com.wavesplatform.dex

import java.io.File
import java.security.Security
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ThreadLocalRandom, TimeoutException}

import akka.Done
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{typed, ActorRef, ActorSystem, CoordinatedShutdown, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.respondWithHeader
import akka.pattern.{ask, gracefulStop, CircuitBreaker}
import akka.stream.Materializer
import akka.util.Timeout
import cats.data.EitherT
import cats.instances.future.catsStdInstancesForFuture
import cats.syntax.functor._
import cats.syntax.option._
import ch.qos.logback.classic.LoggerContext
import com.typesafe.config._
import com.wavesplatform.dex.actors.ActorSystemOps.ImplicitOps
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.orderbook.{AggregatedOrderBookActor, OrderBookActor, OrderBookSnapshotStoreActor}
import com.wavesplatform.dex.actors.tx.{BroadcastExchangeTransactionActor, WriteExchangeTransactionActor}
import com.wavesplatform.dex.actors.{MatcherActor, OrderBookAskAdapter, OrderEventsCoordinatorActor, RootActorSystem, SpendableBalancesActor}
import com.wavesplatform.dex.api.http.headers.{CustomMediaTypes, MatcherHttpServer}
import com.wavesplatform.dex.api.http.routes.{MatcherApiRoute, MatcherApiRouteV1}
import com.wavesplatform.dex.api.http.{CompositeHttpService, OrderBookHttpInfo}
import com.wavesplatform.dex.api.routes.ApiRoute
import com.wavesplatform.dex.api.ws.actors.{WsExternalClientDirectoryActor, WsInternalBroadcastActor}
import com.wavesplatform.dex.api.ws.routes.MatcherWebSocketRoute
import com.wavesplatform.dex.app._
import com.wavesplatform.dex.caches.{MatchingRulesCache, OrderFeeSettingsCache, RateCache}
import com.wavesplatform.dex.db._
import com.wavesplatform.dex.db.leveldb.openDB
import com.wavesplatform.dex.domain.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.utils.{EitherExt2, LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.effect.{liftValueAsync, FutureResult}
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError}
import com.wavesplatform.dex.grpc.integration.WavesClientBuilder
import com.wavesplatform.dex.grpc.integration.clients.{MatcherExtensionAssetsWatchingClient, WavesBlockchainClient}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.history.HistoryRouterActor
import com.wavesplatform.dex.logs.SystemInformationReporter
import com.wavesplatform.dex.model.{AssetPairBuilder, ExchangeTransactionCreator, Fee, OrderValidator, ValidationStages}
import com.wavesplatform.dex.queue._
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.time.NTP
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import monix.execution.ExecutionModel
import mouse.any.anySyntaxMouse
import org.slf4j.LoggerFactory
import pureconfig.ConfigSource

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{blocking, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Application(settings: MatcherSettings, config: Config)(implicit val actorSystem: ActorSystem) extends ScorexLogging {

  private val monixScheduler = monix.execution.Scheduler.Implicits.global.withExecutionModel(ExecutionModel.AlwaysAsyncExecution)
  private val grpcExecutionContext = actorSystem.dispatchers.lookup("akka.actor.grpc-dispatcher")

  private val cs = CoordinatedShutdown(actorSystem)

  implicit private val materializer = Materializer.matFromSystem(actorSystem)
  cs.addTask(CoordinatedShutdown.PhaseBeforeActorSystemTerminate, "Materializer") { () =>
    materializer.shutdown()
    Future.successful(Done)
  }

  private val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())
  private val matchingRulesCache = new MatchingRulesCache(settings)
  private val orderFeeSettingsCache = new OrderFeeSettingsCache(settings.orderFee)

  private val maybeApiKeyHash: Option[Array[Byte]] = Option(settings.restApi.apiKeyHash) filter (_.nonEmpty) map Base58.decode
  private val processConsumedTimeout = new Timeout(settings.processConsumedTimeout * 2)

  private val matcherKeyPair = AccountStorage.load(settings.accountStorage).map(_.keyPair).explicitGet().unsafeTap { x =>
    log.info(s"The DEX's public key: ${Base58.encode(x.publicKey.arr)}, account address: ${x.publicKey.toAddress.stringRepr}")
  }

  private def matcherPublicKey: PublicKey = matcherKeyPair
  @volatile private var lastProcessedOffset = -1L

  @volatile private var status: MatcherStatus = MatcherStatus.Starting
  cs.addJvmShutdownHook(setStatus(MatcherStatus.Stopping))

  @volatile private var hasMatcherAccountScript = false
  private val orderBooks = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val snapshotsRestored = Promise[ValidatedCommandWithMeta.Offset]() // Earliest offset among snapshots

  private val wavesLifted: FutureResult[BriefAssetDescription] = liftValueAsync(BriefAssetDescription.wavesDescription)

  private val time = new NTP(settings.ntpServer)
  cs.addTask(CoordinatedShutdown.PhaseActorSystemTerminate, "NTP") { () =>
    Future { blocking(time.close()); Done }
  }

  private val db = openDB(settings.dataDirectory)
  cs.addTask(CoordinatedShutdown.PhaseActorSystemTerminate, "DB") { () =>
    Future { blocking(db.close()); Done }
  }

  private val assetPairsDB = AssetPairsDB(db)
  private val orderBookSnapshotDB = OrderBookSnapshotDB(db)
  private val orderDB = OrderDB(settings.orderDb, db)
  private val assetsCache = AssetsStorage.cache(AssetsStorage.levelDB(db))
  private val rateCache = RateCache(db)

  implicit private val errorContext: ErrorFormatterContext = ErrorFormatterContext.fromOptional(assetsCache.get(_: Asset).map(_.decimals))

  private val matcherQueue: MatcherQueue = settings.eventsQueue.`type` match {
    case "local" =>
      log.info("Commands will be stored locally")
      new LocalMatcherQueue(settings.eventsQueue.local, new LocalQueueStore(db), time)

    case "kafka" =>
      log.info("Commands will be stored in Kafka")
      new KafkaMatcherQueue(settings.eventsQueue.kafka)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  cs.addTask(CoordinatedShutdown.PhaseServiceStop, "Queue") { () =>
    matcherQueue.close(5.seconds).map(_ => Done)
  }

  private val orderBookAskAdapter = new OrderBookAskAdapter(orderBooks, settings.actorResponseTimeout)

  private val orderBookHttpInfo =
    new OrderBookHttpInfo(settings.orderBookHttp, orderBookAskAdapter, time, assetsCache.get(_).map(_.decimals))

  private val transactionCreator = new ExchangeTransactionCreator(
    matcherKeyPair,
    settings.exchangeTxBaseFee,
    hasMatcherAccountScript,
    assetsCache.unsafeGetHasScript
  )

  private val wavesBlockchainAsyncClient = new MatcherExtensionAssetsWatchingClient(
    settings = settings.wavesBlockchainClient,
    underlying = WavesClientBuilder.async(
      settings.wavesBlockchainClient,
      matcherPublicKey,
      monixScheduler = monixScheduler,
      grpcExecutionContext = grpcExecutionContext
    ),
    assetsStorage = assetsCache
  )

  cs.addTask(CoordinatedShutdown.PhaseServiceStop, "Blockchain client") { () =>
    wavesBlockchainAsyncClient.close().map(_ => Done)
  }

  private val pairBuilder =
    new AssetPairBuilder(settings, getAndCacheDescription(assetsCache, wavesBlockchainAsyncClient, _), settings.blacklistedAssets)

  private val txWriterRef = actorSystem.actorOf(WriteExchangeTransactionActor.props(db), WriteExchangeTransactionActor.name)

  private val wavesNetTxBroadcasterRef = actorSystem.actorOf(
    BroadcastExchangeTransactionActor
      .props(
        settings.exchangeTransactionBroadcast,
        time,
        wavesBlockchainAsyncClient.areKnown,
        wavesBlockchainAsyncClient.broadcastTx
      ),
    "exchange-transaction-broadcast"
  )

  private val wsInternalBroadcastRef: typed.ActorRef[WsInternalBroadcastActor.Command] = actorSystem.spawn(
    WsInternalBroadcastActor(settings.webSockets.internalBroadcast),
    "ws-internal-broadcast"
  )

  private val externalClientDirectoryRef: typed.ActorRef[WsExternalClientDirectoryActor.Message] =
    actorSystem.spawn(WsExternalClientDirectoryActor(), s"ws-external-cd-${ThreadLocalRandom.current().nextInt(Int.MaxValue)}")

  private val orderBookSnapshotStoreRef: ActorRef = actorSystem.actorOf(
    OrderBookSnapshotStoreActor.props(orderBookSnapshotDB),
    "order-book-snapshot-store"
  )

  private val historyRouterRef =
    if (settings.orderHistory.enable) actorSystem.actorOf(
      HistoryRouterActor.props(assetsCache.unsafeGetDecimals, settings.postgres, settings.orderHistory),
      "history-router"
    ).some
    else none

  private val addressDirectoryRef =
    actorSystem.actorOf(AddressDirectoryActor.props(orderDB, mkAddressActorProps, historyRouterRef), AddressDirectoryActor.name)

  private val storeBreaker = new CircuitBreaker(
    actorSystem.scheduler,
    maxFailures = settings.eventsQueue.circuitBreaker.maxFailures,
    callTimeout = settings.eventsQueue.circuitBreaker.callTimeout,
    resetTimeout = settings.eventsQueue.circuitBreaker.resetTimeout
  )

  private def storeCommand(payload: ValidatedCommand): Future[Option[ValidatedCommandWithMeta]] =
    storeBreaker.withCircuitBreaker(matcherQueue.store(payload))

  private def mkAddressActorProps(address: Address, started: Boolean): Props = AddressActor.props(
    address,
    time,
    orderDB,
    ValidationStages.mkSecond(wavesBlockchainAsyncClient, orderBookAskAdapter),
    storeCommand,
    started,
    spendableBalancesRef,
    settings.addressActor
  )

  private val spendableBalancesRef = actorSystem.actorOf(SpendableBalancesActor.props(wavesBlockchainAsyncClient, addressDirectoryRef))

  private val orderEventsCoordinatorRef = actorSystem.spawn(
    behavior = OrderEventsCoordinatorActor.apply(
      addressDirectoryRef = addressDirectoryRef,
      spendableBalancesRef = spendableBalancesRef,
      txWriterRef = txWriterRef,
      broadcastRef = wavesNetTxBroadcasterRef,
      createTransaction = transactionCreator.createTransaction,
      isTransactionKnown = txId => wavesBlockchainAsyncClient.areKnown(List(txId)).map(_.getOrElse(txId, false))
    ),
    name = "events-coordinator"
  )

  private val matcherActorRef: ActorRef = {
    def mkOrderBookProps(assetPair: AssetPair, matcherActor: ActorRef): Props = {
      matchingRulesCache.setCurrentMatchingRuleForNewOrderBook(assetPair, lastProcessedOffset, errorContext.unsafeAssetDecimals)
      OrderBookActor.props(
        OrderBookActor.Settings(AggregatedOrderBookActor.Settings(settings.webSockets.externalClientHandler.messagesInterval)),
        matcherActor,
        orderEventsCoordinatorRef,
        orderBookSnapshotStoreRef,
        wsInternalBroadcastRef,
        assetPair,
        time,
        matchingRules = matchingRulesCache.getMatchingRules(assetPair, errorContext.unsafeAssetDecimals),
        updateCurrentMatchingRules = actualMatchingRule => matchingRulesCache.updateCurrentMatchingRule(assetPair, actualMatchingRule),
        normalizeMatchingRule = denormalizedMatchingRule => denormalizedMatchingRule.normalize(assetPair, errorContext.unsafeAssetDecimals),
        getMakerTakerFeeByOffset = Fee.getMakerTakerFeeByOffset(orderFeeSettingsCache),
        restrictions = settings.orderRestrictions.get(assetPair) // TODO Move this and webSocketSettings to OrderBook's settings
      )
    }

    actorSystem.actorOf(
      MatcherActor.props(
        settings,
        assetPairsDB,
        {
          case Right(startOffset) => snapshotsRestored.success(startOffset)
          case Left(msg) => snapshotsRestored.failure(RecoveryError(msg))
        },
        orderBooks,
        mkOrderBookProps,
        assetsCache.get(_),
        pairBuilder.quickValidateAssetPair
      ),
      MatcherActor.name
    )
  }

  cs.addTask(CoordinatedShutdown.PhaseServiceRequestsDone, "Actors") { () =>
    gracefulStop(matcherActorRef, 3.seconds, MatcherActor.Shutdown).map(_ => Done)
  }

  private val httpApiRouteV0: MatcherApiRoute = {
    val orderValidation = ValidationStages.mkFirst(
      settings,
      matcherPublicKey,
      orderFeeSettingsCache,
      matchingRulesCache,
      rateCache,
      assetsCache,
      wavesBlockchainAsyncClient,
      transactionCreator,
      time,
      orderBookAskAdapter,
      lastProcessedOffset,
      blacklistedAddresses,
      hasMatcherAccountScript
    )(_)

    new MatcherApiRoute(
      pairBuilder,
      matcherPublicKey,
      config,
      matcherActorRef,
      addressDirectoryRef,
      matcherQueue.store,
      p => Option(orderBooks.get()) flatMap (_ get p),
      orderBookHttpInfo,
      getActualTickSize = assetPair => {
        matchingRulesCache.getDenormalizedRuleForNextOrder(assetPair, lastProcessedOffset, assetsCache.unsafeGetDecimals).tickSize
      },
      orderValidation,
      settings,
      () => status,
      orderDB,
      () => lastProcessedOffset,
      () => matcherQueue.lastOffset,
      ExchangeTransactionCreator.getAdditionalFeeForScript(hasMatcherAccountScript),
      maybeApiKeyHash,
      rateCache,
      validatedAllowedOrderVersions = () => {
        Future
          .sequence {
            settings.allowedOrderVersions.map(OrderValidator.checkOrderVersion(_, wavesBlockchainAsyncClient.isFeatureActivated).value)
          }
          .map(_.collect { case Right(version) => version })
      },
      () => orderFeeSettingsCache.getSettingsForOffset(lastProcessedOffset + 1),
      externalClientDirectoryRef
    )
  }

  private val httpApiRouteV1 = MatcherApiRouteV1(pairBuilder, orderBookHttpInfo, () => status, maybeApiKeyHash)

  private val wsApiRoute = new MatcherWebSocketRoute(
    wsInternalBroadcastRef,
    externalClientDirectoryRef,
    addressDirectoryRef,
    matcherActorRef,
    time,
    pairBuilder,
    maybeApiKeyHash,
    settings,
    () => status,
    () => rateCache.getAllRates
  )

  cs.addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, "WebSockets")(() => wsApiRoute.gracefulShutdown().map(_ => Done))

  private val matcherApiRoutes: Seq[ApiRoute] = Seq(httpApiRouteV0, httpApiRouteV1, wsApiRoute)
  private val matcherApiTypes: Set[Class[_]] = matcherApiRoutes.map(_.getClass).toSet

  private val startGuard = for {
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

    _ = {
      log.info("Start watching Node updates")
      wavesBlockchainAsyncClient.updates.foreach { updates =>
        orderEventsCoordinatorRef ! OrderEventsCoordinatorActor.Command.ApplyUpdates(updates)
      }(monixScheduler)
    }

    _ <- {
      log.info(s"Preparing HTTP service (Matcher's ID = ${settings.id}) ...")
      // Indirectly initializes matcherActor, so it must be after loadAllKnownAssets
      val combinedRoute = respondWithHeader(MatcherHttpServer(settings.id)) {
        new CompositeHttpService(matcherApiTypes, matcherApiRoutes, settings.restApi).compositeRoute
      }

      log.info(s"Binding REST and WebSocket API ${settings.restApi.address}:${settings.restApi.port} ...")
      http
        .newServerAt(settings.restApi.address, settings.restApi.port)
        .adaptSettings { settings =>
          settings.withParserSettings(settings.parserSettings.withCustomMediaTypes(CustomMediaTypes.`application/hocon`))
        }
        .bind(combinedRoute)
        .map(_.addToCoordinatedShutdown(hardTerminationDeadline = 5.seconds))
    } map { serverBinding =>
      log.info(s"REST and WebSocket API bound to ${serverBinding.localAddress}")
    }

    earliestSnapshotOffset <- {
      log.info("Waiting all snapshots are restored ...")
      waitSnapshotsRestored(settings.snapshotsLoadingTimeout).andThen(_ => log.info("All snapshots are restored"))
    }

    deadline = settings.startEventsProcessingTimeout.fromNow
    (firstQueueOffset, lastOffsetQueue) <- {
      log.info("Getting queue offsets ...")
      val requests = new RepeatableRequests(matcherQueue, deadline)
      requests.firstOffset zip requests.lastOffset
    }

    firstConsumedOffset = earliestSnapshotOffset + 1

    _ <- {
      log.info(s"Offsets: earliest snapshot = $earliestSnapshotOffset, first = $firstQueueOffset, last = $lastOffsetQueue")
      if (lastOffsetQueue < earliestSnapshotOffset)
        Future.failed(RecoveryError("The queue doesn't have messages to recover all orders and continue work. Did you clear the queue?"))
      else if (earliestSnapshotOffset < firstQueueOffset) {
        log.warn(s"The queue doesn't contain required offsets to recover all orders. Check retention settings of the queue. Continue...")
        Future.unit // Otherwise it would be hard to start the matcher
      } else Future.unit
    }

    _ <- Future {
      log.info("Starting consuming")
      matcherQueue.startConsume(firstConsumedOffset, consumeMessages)
    } zip {
      log.info(s"Last queue offset is $lastOffsetQueue")
      waitOffsetReached(lastOffsetQueue, deadline)
    }
  } yield {
    log.info("Last offset has been reached, switching to a normal mode")
    orderEventsCoordinatorRef ! OrderEventsCoordinatorActor.Command.Start
    addressDirectoryRef ! AddressDirectoryActor.StartWork
  }

  startGuard.onComplete {
    case Success(_) => setStatus(MatcherStatus.Working)
    case Failure(e: ApplicationStopReason) => forceStopApplication(e)
    case Failure(e) =>
      log.error(s"Can't start matcher: ${e.getMessage}", e)
      forceStopApplication(StartingMatcherError)
  }

  private def loadAllKnownAssets(): Future[Unit] =
    Future(blocking(assetPairsDB.all()).flatMap(_.assets) ++ settings.mentionedAssets).flatMap { assetsToLoad =>
      Future
        .traverse(assetsToLoad)(asset => getDecimalsFromCache(asset).value tupleLeft asset)
        .map { xs =>
          val notFoundAssets = xs.collect { case (id, Left(_)) => id }
          if (notFoundAssets.nonEmpty) {
            log.error(s"Can't load assets: ${notFoundAssets.mkString(", ")}")
            forceStopApplication(NotSynchronizedNodeError)
          }
        }
    }

  private def consumeMessages(xs: Iterable[ValidatedCommandWithMeta]): Future[Unit] =
    if (xs.isEmpty) Future.unit
    else {
      val eventAssets = xs.flatMap(_.command.assets)
      val loadAssets = Future.traverse(eventAssets)(getAndCacheDescription(assetsCache, wavesBlockchainAsyncClient, _).value)

      loadAssets.flatMap { _ =>
        val assetPairs: Set[AssetPair] = xs
          .map { validatedCommandWithMeta =>
            log.debug(s"Consumed $validatedCommandWithMeta")
            matcherActorRef ! validatedCommandWithMeta
            lastProcessedOffset = validatedCommandWithMeta.offset
            validatedCommandWithMeta.command.assetPair
          }
          .to(Set)

        matcherActorRef
          .ask(MatcherActor.PingAll(assetPairs))(processConsumedTimeout)
          .recover { case NonFatal(e) => log.error("PingAll is timed out!", e) }
          .map(_ => ())
      } andThen {
        case Failure(ex) =>
          log.error("Error while event processing occurred: ", ex)
          forceStopApplication(EventProcessingError)
        case _ =>
      }
    }

  private def getDecimalsFromCache(asset: Asset): FutureResult[Int] = getAndCacheDecimals(assetsCache, wavesBlockchainAsyncClient, asset)

  private def setStatus(newStatus: MatcherStatus): Unit = {
    status = newStatus
    log.info(s"Status now is $newStatus")
  }

  private def waitSnapshotsRestored(wait: FiniteDuration): Future[ValidatedCommandWithMeta.Offset] = Future.firstCompletedOf(
    List(
      snapshotsRestored.future,
      actorSystem.timeout(wait).recover { case _ => throw RecoveryError(s"Timeout of $wait for waiting snapshots to restore is out") }
    )
  )

  private def waitOffsetReached(lastQueueOffset: ValidatedCommandWithMeta.Offset, deadline: Deadline): Future[Unit] = {
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

  private def getAndCacheDecimals(assetsCache: AssetsStorage, blockchain: WavesBlockchainClient, asset: Asset): FutureResult[Int] =
    getAndCacheDescription(assetsCache, blockchain, asset).map(_.decimals)(catsStdInstancesForFuture)

  private def getAndCacheDescription(
    assetsCache: AssetsStorage,
    blockchain: WavesBlockchainClient,
    asset: Asset
  ): FutureResult[BriefAssetDescription] =
    asset match {
      case Waves => wavesLifted
      case asset: IssuedAsset =>
        assetsCache.get(asset) match {
          case Some(x) => liftValueAsync[BriefAssetDescription](x)
          case None =>
            EitherT {
              blockchain
                .assetDescription(asset)
                .map {
                  _.toRight[MatcherError](error.AssetNotFound(asset))
                    .map { desc =>
                      BriefAssetDescription(desc.name, desc.decimals, desc.hasScript).unsafeTap(assetsCache.put(asset, _))
                    }
                }
            }
        }
    }

}

object Application {

  def main(args: Array[String]): Unit = {

    // prevents java from caching successful name resolutions, which is needed e.g. for proper NTP server rotation
    // http://stackoverflow.com/a/17219327
    System.setProperty("sun.net.inetaddr.ttl", "0")
    System.setProperty("sun.net.inetaddr.negative.ttl", "0")
    Security.setProperty("networkaddress.cache.ttl", "0")
    Security.setProperty("networkaddress.cache.negative.ttl", "0")

    // specify aspectj to use it's build-in infrastructure
    // http://www.eclipse.org/aspectj/doc/released/pdguide/trace.html
    System.setProperty("org.aspectj.tracing.factory", "default")

    val configFile = args.headOption
    val (config, settings) = loadApplicationConfig(configFile.map(new File(_)))

    // This option is used in logback.xml by default
    if (Option(System.getProperty("waves.dex.root-directory")).isEmpty)
      System.setProperty("waves.dex.root-directory", config.getString("waves.dex.root-directory"))

    val log = LoggerFacade(LoggerFactory getLogger getClass)
    log.info("Starting...")

    RootActorSystem.start("wavesplatform", config) { implicit actorSystem =>
      log.info(s"${s"DEX v${Version.VersionString}"} Blockchain Id: ${settings.addressSchemeCharacter}")

      val cs = CoordinatedShutdown(actorSystem)

      cs.addCancellableJvmShutdownHook {
        SystemInformationReporter.report(config)
      }

      cs.addCancellableTask(CoordinatedShutdown.PhaseBeforeActorSystemTerminate, "Kamon") { () =>
        Kamon.stopModules().map(_ => Done)
      }

      cs.addTask(CoordinatedShutdown.PhaseActorSystemTerminate, "Logger") { () =>
        val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        Future { blocking(loggerContext.stop()); Done }
      }

      new Application(settings, config)
    }
  }

  private def loadApplicationConfig(external: Option[File]): (Config, MatcherSettings) = {

    import com.wavesplatform.dex.settings.loadConfig
    import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps

    import scala.jdk.CollectionConverters._

    val config = loadConfig(external map ConfigFactory.parseFile)
    val scalaContextPath = "scala.concurrent.context"

    config.getConfig(scalaContextPath).toProperties.asScala.foreach { case (k, v) => System.setProperty(s"$scalaContextPath.$k", v) }

    val settings = ConfigSource.fromConfig(config).at("waves.dex").loadOrThrow[MatcherSettings]

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme { override val chainId: Byte = settings.addressSchemeCharacter.toByte }

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    Kamon.reconfigure(config)

    if (config.getBoolean("kamon.enable")) Kamon.registerModule("InfluxDB", new InfluxDBReporter())

    (config, settings)
  }

}
