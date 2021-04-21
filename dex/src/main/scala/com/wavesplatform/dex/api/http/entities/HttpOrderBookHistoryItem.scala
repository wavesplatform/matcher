package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.db.OrderDb.orderIdOrdering
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{AcceptedOrderType, OrderInfo, OrderStatus}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class HttpOrderBookHistoryItem(
  @ApiModelProperty(
    value = "Base58 encoded Order ID",
    dataType = "string",
    example = "7VEr4T9icqopHWLawGAZ7AQiJbjAcnzXn65ekYvbpwnN"
  ) id: Order.Id,
  @ApiModelProperty(
    value = "Order side (sell or buy)",
    dataType = "string",
    example = "sell"
  ) `type`: OrderType,
  @ApiModelProperty(
    value = "Order type (limit or market)",
    dataType = "string",
    example = "limit"
  ) orderType: AcceptedOrderType,
  @ApiModelProperty() amount: Long,
  @ApiModelProperty() filled: Long,
  @ApiModelProperty() price: Long,
  @ApiModelProperty() fee: Long,
  @ApiModelProperty() filledFee: Long,
  @ApiModelProperty(
    value = "Base58 encoded Matcher fee asset ID",
    dataType = "string",
    example = "6RQYnag6kTXaoGi3yPmX9JMpPya8WQntSohisKKCMGr"
  ) feeAsset: Asset,
  @ApiModelProperty() timestamp: Long,
  @ApiModelProperty(
    value = "Status",
    allowableValues = "Accepted, NotFound, PartiallyFilled, Filled, Cancelled"
  ) status: String,
  @ApiModelProperty() assetPair: AssetPair,
  @ApiModelProperty(value = "Average weighed price") avgWeighedPrice: Long,
  @ApiModelProperty(
    value = "Order version",
    dataType = "integer",
    example = "3"
  ) version: Byte,
  @ApiModelProperty(value = "Total executed price assets") totalExecutedPriceAssets: Long
)

object HttpOrderBookHistoryItem {

  implicit val httpOrderBookHistoryItemFormat: OFormat[HttpOrderBookHistoryItem] = Json.format

  val httpOrderBookHistoryItemOrdering: Ordering[HttpOrderBookHistoryItem] = orderIdOrdering.on(item => (item.id, item.timestamp))

  def fromOrderInfo(id: Order.Id, info: OrderInfo[OrderStatus]): HttpOrderBookHistoryItem = HttpOrderBookHistoryItem(
    id = id,
    `type` = info.side,
    orderType = info.orderType,
    amount = info.amount,
    filled = info.status.filledAmount,
    price = info.price,
    fee = info.matcherFee,
    filledFee = info.status.filledFee,
    feeAsset = info.feeAsset,
    timestamp = info.timestamp,
    status = info.status.name,
    assetPair = info.assetPair,
    avgWeighedPrice = info.avgWeighedPrice,
    version = info.orderVersion,
    totalExecutedPriceAssets = info.totalExecutedPriceAssets
  )

}
