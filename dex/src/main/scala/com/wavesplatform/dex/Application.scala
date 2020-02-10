package com.wavesplatform.dex

import java.io.File
import java.security.Security
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import ch.qos.logback.classic.LoggerContext
import com.typesafe.config._
import com.wavesplatform.dex.actors.RootActorSystem
import com.wavesplatform.dex.domain.account.AddressScheme
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.util.SystemInformationReporter
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import net.ceedubs.ficus.Ficus._
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success}

class Application(settings: MatcherSettings)(implicit val actorSystem: ActorSystem) extends ScorexLogging {
  app =>

  private implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  private val shutdownInProgress = new AtomicBoolean(false)
  private val matcher: Matcher   = new Matcher(settings)
  matcher.start()

  // on unexpected shutdown
  sys.addShutdownHook {
    shutdown()
  }

  def shutdown(): Unit =
    if (shutdownInProgress.compareAndSet(false, true)) {
      log.info("Shutting down initiated")

      log.info("Shutting down reporters...")
      val kamonShutdown = Kamon.stopAllReporters().andThen {
        case Success(_) => log.info("Reporters stopped")
        case Failure(e) => log.error("Failed to stop reporters", e)
      }

      val task = matcher
        .shutdown()
        .zip(kamonShutdown)
        .andThen {
          case Success(_) => log.info("Shutdown complete")
          case Failure(e) => log.error("Can't stop DEX correctly", e)
        }

      Await.ready(task, Duration.Inf)
      val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      loggerContext.stop()
    }
}

object Application {

  private[wavesplatform] def loadApplicationConfig(external: Option[File] = None): (Config, MatcherSettings) = {

    import com.wavesplatform.dex.settings.loadConfig
    import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps

    import scala.collection.JavaConverters._

    val config           = loadConfig(external map ConfigFactory.parseFile)
    val scalaContextPath = "scala.concurrent.context"

    config.getConfig(scalaContextPath).toProperties.asScala.foreach { case (k, v) => System.setProperty(s"$scalaContextPath.$k", v) }

    val settings = config.as[MatcherSettings]("waves.dex")(MatcherSettings.valueReader)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme { override val chainId: Byte = settings.addressSchemeCharacter.toByte }

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

    // This option is used in logback.xml by default
    if (Option(System.getProperty("waves.dex.root-directory")).isEmpty)
      System.setProperty("waves.dex.root-directory", config.getString("waves.dex.root-directory"))

    val log                = LoggerFacade(LoggerFactory getLogger getClass)

    log.info("Starting...")
    sys.addShutdownHook { SystemInformationReporter.report(config) }

    RootActorSystem.start("wavesplatform", config) { implicit actorSystem =>
      log.info(s"${s"DEX v${Version.VersionString}"} Blockchain Id: ${settings.addressSchemeCharacter}")
      new Application(settings)
    }
  }
}
