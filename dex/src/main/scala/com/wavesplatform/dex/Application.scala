package com.wavesplatform.dex

import java.io.File
import java.security.Security
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import com.typesafe.config._
import com.wavesplatform.dex.actors.RootActorSystem
import com.wavesplatform.dex.domain.account.{Address, AddressScheme}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.grpc.integration.DEXClient
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.util.SystemInformationReporter
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import monix.reactive.subjects.ConcurrentSubject
import net.ceedubs.ficus.Ficus._
import org.slf4j.LoggerFactory

import scala.concurrent.Await
import scala.concurrent.duration._

class Application(settings: MatcherSettings)(implicit val actorSystem: ActorSystem) extends ScorexLogging {
  app =>

  import monix.execution.Scheduler.Implicits.{global => scheduler}

  private implicit val materializer: ActorMaterializer = ActorMaterializer()

  private val grpcExecutionContext = actorSystem.dispatchers.lookup("akka.actor.grpc-dispatcher")

  private val spendableBalanceChanged = ConcurrentSubject.publish[(Address, Asset)]

  private var matcher: Matcher = _

  def run(): Unit = {

    val gRPCExtensionClient =
      new DEXClient(
        s"${settings.wavesNodeGrpc.host}:${settings.wavesNodeGrpc.port}",
        settings.defaultGrpcCachesExpiration
      )(scheduler, grpcExecutionContext)

    matcher = new Matcher(settings, gRPCExtensionClient)
    matcher.start()

    // on unexpected shutdown
    sys.addShutdownHook {
      Await.ready(Kamon.stopAllReporters(), 20.seconds)
    }
  }

  private val shutdownInProgress             = new AtomicBoolean(false)
  @volatile var serverBinding: ServerBinding = _

  def shutdown(): Unit =
    if (shutdownInProgress.compareAndSet(false, true)) {
      Await.ready(matcher.shutdown(), 5.minutes) // @TODO settings

      spendableBalanceChanged.onComplete()

      log.info("Shutdown complete")
    }
}

object Application {

  private[wavesplatform] def loadApplicationConfig(external: Option[File] = None): (Config, MatcherSettings) = {

    import com.wavesplatform.dex.settings.loadConfig

    val config   = loadConfig(external map ConfigFactory.parseFile)
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

    startDEX(args.headOption)
  }

  private[this] def startDEX(configFile: Option[String]): Unit = {

    val (config, settings) = loadApplicationConfig { configFile.map(new File(_)) }
    val log                = LoggerFacade(LoggerFactory getLogger getClass)

    log.info("Starting...")
    sys.addShutdownHook { SystemInformationReporter.report(config) }

    RootActorSystem.start("wavesplatform", config) { implicit actorSystem =>
      log.info(s"${s"Waves v${Version.VersionString}"} Blockchain Id: ${settings.addressSchemeCharacter}")
      new Application(settings).run()
    }
  }
}
