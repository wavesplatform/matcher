package com.wavesplatform.dex.api

import com.github.ghik.silencer.silent
import com.wavesplatform.dex.domain.order.{Order, OrderJson}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, OFormat}

@silent("deprecated")
case class ApiSuccessfulPlace(
    @ApiModelProperty(value = "Order", dataType = "com.wavesplatform.dex.domain.order.OrderV3") message: Order,
    @ApiModelProperty(value = "Success flag") success: Boolean = true,
    @deprecated(message = "This field is unnecessary", since = "1.2.0")
    @ApiModelProperty(value = "Status", example = "OrderAccepted") status: String = "OrderAccepted",
)
object ApiSuccessfulPlace {
  private implicit val orderFormat: Format[Order]                    = OrderJson.orderFormat // TODO
  implicit val apiSuccessfulPlaceFormat: OFormat[ApiSuccessfulPlace] = Json.format
}
