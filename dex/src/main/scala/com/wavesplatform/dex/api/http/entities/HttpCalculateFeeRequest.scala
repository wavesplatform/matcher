package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.OrderType
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}

@ApiModel(value = "HttpCalculateFeeRequest")
final case class HttpCalculateFeeRequest(
  @ApiModelProperty(
    value = "Order type (sell or buy)",
    dataType = "string",
    example = "sell",
    required = true
  )
  orderType: OrderType,
  amount: Long,
  price: Long
)

object HttpCalculateFeeRequest {
  implicit val formats: OFormat[HttpCalculateFeeRequest] = Json.format[HttpCalculateFeeRequest]
}
