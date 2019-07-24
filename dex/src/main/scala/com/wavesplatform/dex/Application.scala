package com.wavesplatform.dex

import java.io.File
import java.security.Security
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.typesafe.config._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.api.grpc.{TransactionsApiGrpc, TransactionsRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.waves.WavesBlockchainContext
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.network._
import com.wavesplatform.state.{AssetDescription, VolumeAndFee}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, SystemInformationReporter, UtilApp}
import com.wavesplatform.utx.UtxPool
import io.grpc.ManagedChannelBuilder
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import net.ceedubs.ficus.Ficus._
import org.slf4j.LoggerFactory

import scala.concurrent.Await
import scala.concurrent.duration._

class Application(settings: MatcherSettings)(implicit val actorSystem: ActorSystem) extends ScorexLogging {
  app =>

  import monix.execution.Scheduler.Implicits.{global => scheduler}

  private implicit val materializer: ActorMaterializer = ActorMaterializer()

  private val spendableBalanceChanged = ConcurrentSubject.publish[(Address, Asset)]

  private var matcher: Matcher = _

  def run(): Unit = {
    val extensionContext = new WavesBlockchainContext {
//      private val channel = ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
//      private val transactions = TransactionsApiGrpc.blockingStub(channel)

      override def hasTx(tx: Transaction): Boolean = ??? //{
//        transactions.getTransactions(TransactionsRequest())
//      }

      override def broadcastTx(tx: Transaction): Unit = ???

      override def isFeatureActivated(id: Short): Boolean = ???

      override def assetDescription(asset: IssuedAsset): Option[AssetDescription] = ???

      override def hasScript(asset: IssuedAsset): Boolean = ???

      override def runScript(asset: IssuedAsset, input: Transaction): Either[String, Terms.EVALUATED] = ???

      override def hasScript(address: Address): Boolean = ???

      override def runScript(address: Address, input: Order): Either[String, Terms.EVALUATED] = ???

      override def spendableBalanceChanged: Observable[(Address, Asset)] = app.spendableBalanceChanged

      override def spendableBalance(address: Address, asset: Asset): Long = ???

      override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = ???

      override def putToUtx(tx: Transaction): Boolean = ???
    }

    matcher = new Matcher(settings, extensionContext)
    matcher.start()

    // on unexpected shutdown
    sys.addShutdownHook {
      Await.ready(Kamon.stopAllReporters(), 20.seconds)
      Metrics.shutdown()
    }
  }

  private val shutdownInProgress             = new AtomicBoolean(false)
  @volatile var serverBinding: ServerBinding = _

  def shutdown(utx: UtxPool, network: NS): Unit =
    if (shutdownInProgress.compareAndSet(false, true)) {
      Await.ready(matcher.shutdown(), 5.minutes) // @TODO settings

      spendableBalanceChanged.onComplete()
      utx.close()

      log.info("Shutdown complete")
    }
}

object Application {
  private[wavesplatform] def loadApplicationConfig(external: Option[File] = None): (Config, MatcherSettings) = {
    import com.wavesplatform.settings._

    val config = loadConfig(external.map(ConfigFactory.parseFile))

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))

    val settings = config.as[MatcherSettings]("waves.dex")(MatcherSettings.valueReader)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.addressSchemeCharacter.toByte
    }

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    Kamon.reconfigure(config)

    if (config.getBoolean("kamon.enable")) {
      Kamon.addReporter(new InfluxDBReporter())
      SystemMetrics.startCollecting()
    }

    (config, settings)
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
    val (config, settings) = loadApplicationConfig(configFile.map(new File(_)))

    val log = LoggerFacade(LoggerFactory.getLogger(getClass))
    log.info("Starting...")
    sys.addShutdownHook {
      SystemInformationReporter.report(config)
    }

    RootActorSystem.start("wavesplatform", config) { implicit actorSystem =>
      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.addressSchemeCharacter}")
      new Application(settings).run()
    }
  }
}
