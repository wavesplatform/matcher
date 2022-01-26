package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.OrderType
import play.api.libs.json.{Json, OFormat}

final case class HttpCalculateFeeRequest(orderType: OrderType, amount: Long, price: Long)

object HttpCalculateFeeRequest {
  implicit val formats: OFormat[HttpCalculateFeeRequest] = Json.format[HttpCalculateFeeRequest]
}
