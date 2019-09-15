package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class OrderStatusResponse(status: OrderStatus, filledAmount: Option[Long])
object OrderStatusResponse {
  implicit val format: Format[OrderStatusResponse] = Json.format[OrderStatusResponse]
}
