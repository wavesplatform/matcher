package com.wavesplatform.dex.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import com.wavesplatform.dex.actors.OrderBookAskAdapter
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor.Depth
import com.wavesplatform.dex.actors.orderbook.OrderBookActor.MarketStatus
import com.wavesplatform.dex.api.http.entities.MatcherResponse.toHttpResponse
import com.wavesplatform.dex.api.http.entities.{HttpMarketStatus, HttpOrderBook, OrderBookUnavailable, SimpleResponse}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized}
import com.wavesplatform.dex.time.Time

import scala.concurrent.{ExecutionContext, Future}

class OrderBookHttpInfo(settings: OrderBookHttpInfo.Settings, askAdapter: OrderBookAskAdapter, time: Time, assetDecimals: Asset => Option[Int])(
  implicit ec: ExecutionContext
) {

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

  private def toHttpMarketStatusResponse(ms: MarketStatus): HttpResponse = toHttpResponse(SimpleResponse(HttpMarketStatus fromMarketStatus ms))

  def getHttpView(assetPair: AssetPair, format: DecimalsFormat, depth: Option[Depth]): Future[HttpResponse] =
    askAdapter.getHttpView(assetPair, format, settings.nearestBigger(depth)).map {
      case Right(x) => x.getOrElse(getDefaultHttpView(assetPair, format))
      case Left(e) => toHttpResponse(OrderBookUnavailable(e))
    }

  private def getDefaultHttpView(assetPair: AssetPair, format: DecimalsFormat): HttpResponse = {
    val entity = HttpOrderBook(time.correctedTime(), assetPair, Seq.empty, Seq.empty, assetPairDecimals(assetPair, format))
    HttpResponse(
      entity = HttpEntity(
        ContentTypes.`application/json`,
        HttpOrderBook.toJson(entity)
      )
    )
  }

  private def assetPairDecimals(assetPair: AssetPair, format: DecimalsFormat): Option[(Depth, Depth)] = format match {
    case Denormalized => assetDecimals(assetPair.amountAsset).zip(assetDecimals(assetPair.priceAsset))
    case _ => None
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
