package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.order.Order
import play.api.libs.json.{Format, Json}

case class ApiSuccessfulCancel(
    orderId: Order.Id,
    success: Boolean = true,
    @deprecated(message = "This field is unnecessary", since = "1.2.0") status: String = "OrderCanceled",
)
object ApiSuccessfulCancel {
  implicit val apiSuccessfulCancelFormat: Format[ApiSuccessfulCancel] = Json.format
}
