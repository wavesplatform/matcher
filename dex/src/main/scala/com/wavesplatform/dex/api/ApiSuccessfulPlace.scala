package com.wavesplatform.dex.api

import com.github.ghik.silencer.silent
import com.wavesplatform.dex.domain.order.{Order, OrderJson}
import play.api.libs.json.{Format, Json}

@silent("deprecated")
case class ApiSuccessfulPlace(
    message: Order,
    success: Boolean = true,
    @deprecated(message = "This field is unnecessary", since = "1.2.0") status: String = "OrderAccepted",
)
object ApiSuccessfulPlace {
  private implicit val orderFormat: Format[Order]                   = OrderJson.orderFormat // TODO
  implicit val apiSuccessfulPlaceFormat: Format[ApiSuccessfulPlace] = Json.format
}
