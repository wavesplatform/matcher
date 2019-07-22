package com.wavesplatform.dex

import java.io.File
import java.security.Security
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import cats.instances.all._
import com.typesafe.config._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import com.wavesplatform.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import com.wavesplatform.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.api.http.NxtConsensusApiRoute
import com.wavesplatform.db.openDB
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.api.ActivationApiRoute
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.http.{DebugApiRoute, NodeApiRoute, WavesApiRoute}
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.network.RxExtensionLoader.RxExtensionLoaderShutdownHook
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.appender.{BlockAppender, ExtensionAppender, MicroblockAppender}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.{LoggerFacade, NTP, ScorexLogging, SystemInformationReporter, Time, UtilApp}
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.execution.Scheduler._
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.influxdb.dto.Point
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.util.control.NonFatal

class Application(val actorSystem: ActorSystem, val settings: MatcherSettings, configRoot: ConfigObject, time: NTP) extends ScorexLogging {
  app =>

  import monix.execution.Scheduler.Implicits.{global => scheduler}
  private[this] val apiScheduler = Scheduler(actorSystem.dispatcher)

  private val LocalScoreBroadcastDebounce = 1.second

  private val spendableBalanceChanged = ConcurrentSubject.publish[(Address, Asset)]

  private val wallet: Wallet = try {
    Wallet(settings.walletSettings)
  } catch {
    case NonFatal(e) =>
      log.error(s"Failed to open wallet file '${settings.walletSettings.file.get.getAbsolutePath}", e)
      throw e
  }

  private var rxExtensionLoaderShutdown: Option[RxExtensionLoaderShutdownHook] = None
  private var maybeUtx: Option[UtxPool]                                        = None
  private var maybeNetwork: Option[NS]                                         = None

  def apiShutdown(): Unit = {
    for {
      u <- maybeUtx
      n <- maybeNetwork
    } yield shutdown(u, n)
  }

  private var matcher: Matcher = _

  def run(): Unit = {
    if (wallet.privateKeyAccounts.isEmpty)
      wallet.generateNewAccounts(1)

    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels            = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val utxStorage             = new UtxPoolImpl(time, blockchainUpdater, spendableBalanceChanged, settings.utxSettings)
    maybeUtx = Some(utxStorage)

    val knownInvalidBlocks = new InvalidBlockStorageImpl(settings.synchronizationSettings.invalidBlocksStorage)

    val pos = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)

    val miner =
      if (settings.minerSettings.enable)
        new MinerImpl(allChannels, blockchainUpdater, settings, time, utxStorage, wallet, pos, minerScheduler, appenderScheduler)
      else Miner.Disabled

    val processBlock =
      BlockAppender(blockchainUpdater, time, utxStorage, pos, settings, allChannels, peerDatabase, miner, appenderScheduler) _

    val processFork =
      ExtensionAppender(blockchainUpdater, utxStorage, pos, time, settings, knownInvalidBlocks, peerDatabase, miner, allChannels, appenderScheduler) _
    val processMicroBlock =
      MicroblockAppender(blockchainUpdater, utxStorage, allChannels, peerDatabase, appenderScheduler) _

    import blockchainUpdater.lastBlockInfo

    val lastScore = lastBlockInfo
      .map(_.score)
      .distinctUntilChanged
      .share(scheduler)

    lastScore
      .debounce(LocalScoreBroadcastDebounce)
      .foreach { x =>
        allChannels.broadcast(LocalScoreChanged(x))
      }(scheduler)

    val historyReplier = new HistoryReplier(blockchainUpdater, settings.synchronizationSettings, historyRepliesScheduler)
    val network =
      NetworkServer(settings, lastBlockInfo, blockchainUpdater, historyReplier, utxStorage, peerDatabase, allChannels, establishedConnections)
    maybeNetwork = Some(network)
    val (signatures, blocks, blockchainScores, microblockInvs, microblockResponses, transactions) = network.messages

    val timeoutSubject: ConcurrentSubject[Channel, Channel] = ConcurrentSubject.publish[Channel]

    val (syncWithChannelClosed, scoreStatsReporter) = RxScoreObserver(
      settings.synchronizationSettings.scoreTTL,
      1.second,
      blockchainUpdater.score,
      lastScore,
      blockchainScores,
      network.closedChannels,
      timeoutSubject,
      scoreObserverScheduler
    )
    val (microblockDatas, mbSyncCacheSizes) = MicroBlockSynchronizer(
      settings.synchronizationSettings.microBlockSynchronizer,
      peerDatabase,
      lastBlockInfo.map(_.id),
      microblockInvs,
      microblockResponses,
      microblockSynchronizerScheduler
    )
    val (newBlocks, extLoaderState, sh) = RxExtensionLoader(
      settings.synchronizationSettings.synchronizationTimeout,
      Coeval(blockchainUpdater.lastBlockIds(settings.synchronizationSettings.maxRollback)),
      peerDatabase,
      knownInvalidBlocks,
      blocks,
      signatures,
      syncWithChannelClosed,
      extensionLoaderScheduler,
      timeoutSubject
    ) { case (c, b) => processFork(c, b.blocks) }

    rxExtensionLoaderShutdown = Some(sh)

    UtxPoolSynchronizer.start(utxStorage,
                              settings.synchronizationSettings.utxSynchronizer,
                              allChannels,
                              transactions,
                              blockchainUpdater.lastBlockInfo)

    val microBlockSink = microblockDatas
      .mapTask(scala.Function.tupled(processMicroBlock))

    val blockSink = newBlocks
      .mapTask(scala.Function.tupled(processBlock))

    Observable.merge(microBlockSink, blockSink).subscribe()

    lastBlockInfo
      .map(_.height)
      .distinctUntilChanged
      .whileBusyDropEvents
      .doOnNextTask(_ => utxStorage.cleanupTask)
      .subscribe()

    miner.scheduleMining()

    for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }

    implicit val as: ActorSystem                 = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val extensionContext = new Context {
      override def settings: WavesSettings                               = app.settings
      override def blockchain: Blockchain                                = app.blockchainUpdater
      override def time: Time                                            = app.time
      override def wallet: Wallet                                        = app.wallet
      override def utx: UtxPool                                          = utxStorage
      override def broadcastTx(tx: Transaction): Unit                    = allChannels.broadcastTx(tx, None)
      override def spendableBalanceChanged: Observable[(Address, Asset)] = app.spendableBalanceChanged
      override def actorSystem: ActorSystem                              = app.actorSystem
    }

    matcher = new Matcher(extensionContext)
    matcher.start()

    // on unexpected shutdown
    sys.addShutdownHook {
      Await.ready(Kamon.stopAllReporters(), 20.seconds)
      Metrics.shutdown()
      shutdown(utxStorage, network)
    }
  }

  private val shutdownInProgress             = new AtomicBoolean(false)
  @volatile var serverBinding: ServerBinding = _

  def shutdown(utx: UtxPool, network: NS): Unit =
    if (shutdownInProgress.compareAndSet(false, true)) {
      Await.ready(matcher.shutdown(), settings.extensionsShutdownTimeout)

      spendableBalanceChanged.onComplete()
      utx.close()

      log.info("Shutdown complete")
    }
}

object Application {
  private[wavesplatform] def loadApplicationConfig(external: Option[File] = None): WavesSettings = {
    import com.wavesplatform.settings._

    val config = loadConfig(external.map(ConfigFactory.parseFile))

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))

    val settings = WavesSettings.fromRootConfig(config)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    Kamon.reconfigure(config)

    if (config.getBoolean("kamon.enable")) {
      Kamon.addReporter(new InfluxDBReporter())
      SystemMetrics.startCollecting()
    }

    settings
  }

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

    args.headOption.getOrElse("") match {
      case "util"                   => UtilApp.main(args.tail)
      case "help" | "--help" | "-h" => println("Usage: waves <config> | export | import | explore | util")
      case _                        => startNode(args.headOption)
    }
  }

  private[this] def startNode(configFile: Option[String]): Unit = {
    import com.wavesplatform.settings.Constants
    val settings = loadApplicationConfig(configFile.map(new File(_)))

    val log = LoggerFacade(LoggerFactory.getLogger(getClass))
    log.info("Starting...")
    sys.addShutdownHook {
      SystemInformationReporter.report(settings.config)
    }

    val time             = new NTP(settings.ntpServer)
    val isMetricsStarted = Metrics.start(settings.metrics, time)

    RootActorSystem.start("wavesplatform", settings.config) { actorSystem =>
      import actorSystem.dispatcher
      isMetricsStarted.foreach { started =>
        if (started) {
          import settings.synchronizationSettings.microBlockSynchronizer
          import settings.{minerSettings => miner}

          Metrics.write(
            Point
              .measurement("config")
              .addField("miner-micro-block-interval", miner.microBlockInterval.toMillis)
              .addField("miner-max-transactions-in-key-block", miner.maxTransactionsInKeyBlock)
              .addField("miner-max-transactions-in-micro-block", miner.maxTransactionsInMicroBlock)
              .addField("miner-min-micro-block-age", miner.minMicroBlockAge.toMillis)
              .addField("mbs-wait-response-timeout", microBlockSynchronizer.waitResponseTimeout.toMillis)
          )
        }
      }

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

      new Application(actorSystem, settings, settings.config.root(), time).run()
    }
  }
}
