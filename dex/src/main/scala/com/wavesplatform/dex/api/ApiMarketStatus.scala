package com.wavesplatform.dex.api

import cats.instances.option.catsStdInstancesForOption
import cats.syntax.apply._
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, OWrites, Reads}

case class ApiMarketStatus(lastTrade: Option[ApiLastTrade], bestBid: Option[ApiLevelAgg], bestAsk: Option[ApiLevelAgg])

object ApiMarketStatus {

  def fromMarketStatus(ms: MarketStatus): ApiMarketStatus =
    ApiMarketStatus(ms.lastTrade.map(ApiLastTrade.fromLastTrade), ms.bestBid.map(ApiLevelAgg.fromLevelAgg), ms.bestAsk.map(ApiLevelAgg.fromLevelAgg))

  implicit val apiMarketStatusWrites: OWrites[ApiMarketStatus] = { ms =>
    Json.obj(
      "lastPrice"  -> ms.lastTrade.map(_.price),
      "lastAmount" -> ms.lastTrade.map(_.amount),
      "lastSide"   -> ms.lastTrade.map(_.side.toString),
      "bid"        -> ms.bestBid.map(_.price),
      "bidAmount"  -> ms.bestBid.map(_.amount),
      "ask"        -> ms.bestAsk.map(_.price),
      "askAmount"  -> ms.bestAsk.map(_.amount)
    )
  }

  implicit val apiMarketStatusReads: Reads[ApiMarketStatus] =
    (
      (JsPath \ "lastPrice").readNullable[Long] and
        (JsPath \ "lastAmount").readNullable[Long] and
        (JsPath \ "lastSide").readNullable[OrderType] and
        (JsPath \ "bid").readNullable[Long] and
        (JsPath \ "bidAmount").readNullable[Long] and
        (JsPath \ "ask").readNullable[Long] and
        (JsPath \ "askAmount").readNullable[Long]
    ) { (lastPrice, lastAmount, lastSide, bid, bidAmount, ask, askAmount) =>
      ApiMarketStatus(
        lastTrade = (lastPrice, lastAmount, lastSide).tupled.map(Function.tupled(ApiLastTrade.apply)),
        bestBid = (bidAmount, bid).tupled.map(Function.tupled(ApiLevelAgg.apply)),
        bestAsk = (askAmount, ask).tupled.map(Function.tupled(ApiLevelAgg.apply))
      )
    }
}
