package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.{LastTrade, LevelAgg}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class ApiMarketStatus(@ApiModelProperty(
                             allowEmptyValue = true,
                             dataType = "integer"
                           ) lastPrice: Option[Long],
                           @ApiModelProperty(
                             allowEmptyValue = true,
                             dataType = "integer"
                           ) lastAmount: Option[Long],
                           @ApiModelProperty(
                             value = "Side (sell or buy)",
                             dataType = "string",
                             example = "buy",
                             allowEmptyValue = true
                           )
                           lastSide: Option[OrderType],
                           @ApiModelProperty(
                             allowEmptyValue = true,
                             dataType = "integer"
                           ) bid: Option[Long],
                           @ApiModelProperty(
                             allowEmptyValue = true,
                             dataType = "integer"
                           ) bidAmount: Option[Long],
                           @ApiModelProperty(
                             allowEmptyValue = true,
                             dataType = "integer"
                           ) ask: Option[Long],
                           @ApiModelProperty(
                             allowEmptyValue = true,
                             dataType = "integer"
                           ) askAmount: Option[Long]) {

  @ApiModelProperty(hidden = true)
  val lastTrade: Option[LastTrade] =
    for {
      lp <- lastPrice
      la <- lastAmount
      ls <- lastSide
    } yield LastTrade(lp, la, ls)

  @ApiModelProperty(hidden = true)
  val bestBid: Option[LevelAgg] =
    for {
      bba <- bidAmount
      bbp <- bid
    } yield LevelAgg(bba, bbp)

  @ApiModelProperty(hidden = true)
  val bestAsk: Option[LevelAgg] =
    for {
      baa <- askAmount
      bap <- ask
    } yield LevelAgg(baa, bap)
}

object ApiMarketStatus {

  def fromMarketStatus(ms: MarketStatus): ApiMarketStatus =
    ApiMarketStatus(
      lastPrice = ms.lastTrade.map(_.price),
      lastAmount = ms.lastTrade.map(_.amount),
      lastSide = ms.lastTrade.map(_.side),
      bid = ms.bestBid.map(_.price),
      bidAmount = ms.bestBid.map(_.amount),
      ask = ms.bestAsk.map(_.price),
      askAmount = ms.bestAsk.map(_.amount)
    )

  implicit val apiMarketStatusFormat: OFormat[ApiMarketStatus] = Json.format[ApiMarketStatus]
}
