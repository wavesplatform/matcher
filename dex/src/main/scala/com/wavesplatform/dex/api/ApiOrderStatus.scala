package com.wavesplatform.dex.api

import com.wavesplatform.dex.model.OrderStatus
import play.api.libs.json.{Format, Json}

case class ApiOrderStatus(status: String, filledAmount: Option[Long] = None, filledFee: Option[Long] = None, message: Option[String] = None)
object ApiOrderStatus {
  implicit val apiOrderStatusFormat: Format[ApiOrderStatus] = Json.format

  private val accepted = ApiOrderStatus(OrderStatus.Accepted.name)
  private val notFound = ApiOrderStatus(OrderStatus.NotFound.name, message = Some("The limit order is not found"))
  def from(x: OrderStatus): ApiOrderStatus = x match {
    case OrderStatus.Accepted => accepted
    case OrderStatus.NotFound => notFound
    case _                    => ApiOrderStatus(x.name, Some(x.filledAmount), Some(x.filledFee))
  }
}
