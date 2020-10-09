package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.{Order, OrderJson}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, OFormat}

case class HttpSuccessfulPlace(
  @ApiModelProperty(value = "Order", dataType = "com.wavesplatform.dex.domain.order.OrderV3") message: Order,
  @ApiModelProperty(value = "Success flag") success: Boolean = true,
  @ApiModelProperty(value = "Status", example = "OrderAccepted") status: String =
    "OrderAccepted" // @deprecated(message = "This field is unnecessary", since = "1.2.0")
)

object HttpSuccessfulPlace {
  implicit private val orderFormat: Format[Order] = OrderJson.orderFormat // TODO
  implicit val httpSuccessfulPlaceFormat: OFormat[HttpSuccessfulPlace] = Json.format
}
