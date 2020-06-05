package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.model.LastTrade
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._

case class ApiLastTrade(@ApiModelProperty() price: Long,
                        @ApiModelProperty() amount: Long,
                        @ApiModelProperty(
                          value = "Side (sell or buy)",
                          dataType = "string",
                          example = "buy"
                        ) side: OrderType)

object ApiLastTrade {
  implicit val apiLastTradeFormat: OFormat[ApiLastTrade] = Json.format[ApiLastTrade]
  def fromLastTrade(lt: LastTrade): ApiLastTrade         = ApiLastTrade(lt.price, lt.amount, lt.side)
}
