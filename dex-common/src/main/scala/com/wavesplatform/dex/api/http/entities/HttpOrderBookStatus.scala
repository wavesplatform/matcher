package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.OrderType
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpOrderBookStatus(
  @ApiModelProperty(dataType = "integer") lastPrice: Option[Long],
  @ApiModelProperty(dataType = "integer") lastAmount: Option[Long],
  @ApiModelProperty(
    value = "Side (sell or buy)",
    dataType = "string",
    example = "buy"
  )
  lastSide: Option[OrderType],
  @ApiModelProperty(dataType = "integer") bid: Option[Long],
  @ApiModelProperty(dataType = "integer") bidAmount: Option[Long],
  @ApiModelProperty(dataType = "integer") ask: Option[Long],
  @ApiModelProperty(dataType = "integer") askAmount: Option[Long]
) {

  @ApiModelProperty(hidden = true)
  val lastTrade: Option[HttpLastTrade] =
    for {
      lp <- lastPrice
      la <- lastAmount
      ls <- lastSide
    } yield HttpLastTrade(lp, la, ls)

  @ApiModelProperty(hidden = true)
  val bestBid: Option[HttpLevelAgg] =
    for {
      bba <- bidAmount
      bbp <- bid
    } yield HttpLevelAgg(bba, bbp)

  @ApiModelProperty(hidden = true)
  val bestAsk: Option[HttpLevelAgg] =
    for {
      baa <- askAmount
      bap <- ask
    } yield HttpLevelAgg(baa, bap)

}

object HttpOrderBookStatus {

  def fromMarketStatus(ms: HttpMarketStatus): HttpOrderBookStatus =
    HttpOrderBookStatus(
      lastPrice = ms.lastTrade.map(_.price),
      lastAmount = ms.lastTrade.map(_.amount),
      lastSide = ms.lastTrade.map(_.side),
      bid = ms.bestBid.map(_.price),
      bidAmount = ms.bestBid.map(_.amount),
      ask = ms.bestAsk.map(_.price),
      askAmount = ms.bestAsk.map(_.amount)
    )

  implicit val httpMarketStatusFormat: OFormat[HttpOrderBookStatus] = Json.format[HttpOrderBookStatus]
}
