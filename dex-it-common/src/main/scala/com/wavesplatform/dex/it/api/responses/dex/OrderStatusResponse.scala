package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class OrderStatusResponse(status: OrderStatus, filledAmount: Option[Long], filledFee: Option[Long])
object OrderStatusResponse {
  implicit val format: Format[OrderStatusResponse] = Json.format[OrderStatusResponse]
}
