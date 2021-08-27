package com.wavesplatform.dex.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import cats.instances.future._
import cats.syntax.semigroupal._
import com.wavesplatform.dex.actors.OrderBookAskAdapter
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor.{Depth, MarketStatus}
import com.wavesplatform.dex.api.http.entities.MatcherResponse.toHttpResponse
import com.wavesplatform.dex.api.http.entities.{HttpOrderBook, HttpOrderBookStatus, OrderBookUnavailable, SimpleErrorResponse, SimpleResponse}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.effect.{liftValueAsync, FutureResult}
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized}
import com.wavesplatform.dex.time.Time

import scala.concurrent.{ExecutionContext, Future}

/**
 * @param assetDecimals Returns FutureResult, because we potentially face with unknown assets
 */
class OrderBookHttpInfo(
  settings: OrderBookHttpInfo.Settings,
  askAdapter: OrderBookAskAdapter,
  time: Time,
  assetDecimals: Asset => FutureResult[Int]
)(implicit ec: ExecutionContext) {

  private val emptyMarketStatus = toHttpMarketStatusResponse(MarketStatus(None, None, None))

  def getMarketStatus(assetPair: AssetPair): Future[HttpResponse] =
    askAdapter.getMarketStatus(assetPair).map {
      case Left(e) => toHttpResponse(OrderBookUnavailable(e))
      case Right(maybeMarketStatus) =>
        maybeMarketStatus match {
          case Some(ms) => toHttpMarketStatusResponse(ms)
          case None => emptyMarketStatus
        }
    }

  private def toHttpMarketStatusResponse(ms: MarketStatus): HttpResponse = toHttpResponse(SimpleResponse(HttpOrderBookStatus fromMarketStatus ms))

  def getHttpView(assetPair: AssetPair, format: DecimalsFormat, depth: Option[Depth]): Future[HttpResponse] =
    askAdapter.getHttpView(assetPair, format, settings.nearestBigger(depth)).flatMap {
      case Right(Some(x)) => Future.successful(x)
      case Right(None) => getDefaultHttpView(assetPair, format)
      case Left(e) => Future.successful(toHttpResponse(OrderBookUnavailable(e)))
    }

  private def getDefaultHttpView(assetPair: AssetPair, format: DecimalsFormat): Future[HttpResponse] =
    assetPairDecimals(assetPair, format).value.map {
      case Right(assetPairDecimals) =>
        val entity = HttpOrderBook(time.correctedTime(), assetPair, Seq.empty, Seq.empty, assetPairDecimals)
        HttpResponse(
          entity = HttpEntity(
            ContentTypes.`application/json`,
            HttpOrderBook.toJson(entity)
          )
        )

      case Left(e) => toHttpResponse(SimpleErrorResponse(e))
    }

  private def assetPairDecimals(assetPair: AssetPair, format: DecimalsFormat): FutureResult[Option[(Depth, Depth)]] = format match {
    case Denormalized => assetDecimals(assetPair.amountAsset).product(assetDecimals(assetPair.priceAsset)).map(Some(_))
    case _ => liftValueAsync(None)
  }

}

object OrderBookHttpInfo {

  case class Settings(depthRanges: List[Int], defaultDepth: Option[Int]) {

    def nearestBigger(to: Option[Int]): Int =
      to.orElse(defaultDepth)
        .flatMap(desiredDepth => depthRanges.find(_ >= desiredDepth))
        .getOrElse(depthRanges.max)

  }

}
