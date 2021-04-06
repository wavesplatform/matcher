package com.wavesplatform.dex.tool

import com.wavesplatform.dex.domain.asset.Asset
import java.io.File
import java.util.{Timer, TimerTask}
import java.util.concurrent.{Executors, TimeUnit}

import cats.data.NonEmptyList
import cats.syntax.either._
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.typesafe.config.ConfigFactory.parseFile
import com.wavesplatform.dex.actors.address.BalancesFormatter
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.domain.account.{AddressScheme, PublicKey}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.json
import com.wavesplatform.dex.settings.{loadConfig, MatcherSettings}
import com.wavesplatform.dex.tool.ComparisonTool._
import com.wavesplatform.dex.domain.asset.AssetPair
import play.api.libs.json.Json
import pureconfig.ConfigSource
import sttp.client3.asynchttpclient.future.AsyncHttpClientFutureBackend
import sttp.model.Uri
import sttp.client3._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

class ComparisonTool(settings: Settings) extends ScorexLogging {

  private val timer = new Timer("runTradableBalanceChecks")
  private val threadPool = Executors.newFixedThreadPool(5, new ThreadFactoryBuilder().setDaemon(false).build())

  implicit private val ec = ExecutionContext.fromExecutor(threadPool)
  implicit protected val backend = AsyncHttpClientFutureBackend()(ec)

  @volatile private var strikes = 0

  def run(): Unit = {
    val deadline = settings.checks.duration.fromNow
    def loop(): Unit =
      if (deadline.hasTimeLeft()) {
        runTradableBalanceChecks()
        timer.schedule(
          new TimerTask {
            override def run(): Unit = loop()
          },
          settings.checks.interval.toMillis
        )
      } else {
        timer.cancel()
        Await.ready(backend.close(), 5.seconds)
        threadPool.shutdown()
        threadPool.awaitTermination(5L, TimeUnit.SECONDS)
        log.info("Done")
      }

    loop()
  }

  private def runTradableBalanceChecks(): Unit = {
    val xs =
      for {
        baseUri <- settings.matcherRestApis
        assetPair <- settings.tradableBalanceCheck.assetPairs
        pk <- settings.tradableBalanceCheck.accountPks
      } yield basicRequest
        .get(uri"$baseUri/matcher/orderbook/${assetPair.amountAsset}/${assetPair.priceAsset}/tradableBalance/${pk.toAddress}")
        .response(asString("UTF-8"))
        .send(backend)
        .map(r => ((pk, assetPair), baseUri, parse(r)))

    Future
      .sequence(xs)
      .onComplete {
        case Failure(e) => log.error("Can't check tradable balances", e)
        case Success(responses) =>
          responses
            .groupMap(_._1) { case (_, server, response) => (server, response) }
            .foreach { case ((pk, assetPair), responses) =>
              val (str, areSame) = compareResponses(NonEmptyList.fromList(responses).getOrElse(throw new RuntimeException("Improsibru!")))
              val message =
                s"${pk.toAddress.stringRepr.take(5)}, " +
                s"${assetPair.amountAssetStr.take(5)}-${assetPair.priceAssetStr.take(5)}${if (areSame) " same" else ""}: $str"
              if (!areSame) strikes += 1
              if (strikes >= settings.checks.strike) log.error(message)
              else if (Random.nextFloat() < 0.01) log.info(message)
            }
      }
  }

  private def compareResponses(xs: NonEmptyList[(Uri, TradableBalanceCheckResponse)]): (String, Boolean) = {
    val reference = xs.head._2
    xs.foldLeft((s"$reference", true)) { case (r @ (str, _), (server, response)) =>
      if (response == reference) r
      else (s"$str; $server: $response", false)
    }
  }

  private def parse(response: Response[Either[String, String]]): TradableBalanceCheckResponse =
    response.body match {
      case Left(_) => TradableBalanceCheckResponse.Failed(response.code.code)
      case Right(body) => TradableBalanceCheckResponse.Parsed(Json.parse(body).as[Map[Asset, Long]](json.assetMapFormat[Long]))
    }

}

object ComparisonTool {

  case class Settings(
    checks: ChecksSettings,
    matcherRestApis: List[Uri],
    tradableBalanceCheck: TradableBalanceCheck
  )

  case class ChecksSettings(interval: FiniteDuration, duration: FiniteDuration, strike: Int)
  case class TradableBalanceCheck(accountPks: List[PublicKey], assetPairs: List[AssetPair])

  def apply(confPath: String): ErrorOr[ComparisonTool] = Try {
    val matcherSettings = ConfigSource
      .fromConfig(loadConfig(parseFile(new File(confPath)))).at("waves.dex")
      .loadOrThrow[MatcherSettings]

    AddressScheme.current = new AddressScheme { override val chainId: Byte = matcherSettings.addressSchemeCharacter.toByte }
    new ComparisonTool(matcherSettings.comparisonTool)
  }.toEither.leftMap(ex => s"Cannot load matcher settings by path $confPath: ${ex.getWithStackTrace}")

  sealed private[ComparisonTool] trait TradableBalanceCheckResponse extends Product with Serializable

  private[ComparisonTool] object TradableBalanceCheckResponse {
    case class Failed(httpErrorCode: Int) extends TradableBalanceCheckResponse

    case class Parsed(xs: Map[Asset, Long]) extends TradableBalanceCheckResponse {
      override def toString: String = s"Parsed${BalancesFormatter.format(xs)}"
    }

  }

}
