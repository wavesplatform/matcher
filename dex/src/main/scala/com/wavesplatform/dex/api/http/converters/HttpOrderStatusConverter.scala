package com.wavesplatform.dex.api.http.converters

import cats.syntax.option._
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.model.OrderStatus

object HttpOrderStatusConverter {

  def from(x: OrderStatus): HttpOrderStatus = x match {
    case OrderStatus.Accepted => HttpOrderStatus(Status.Accepted)
    case OrderStatus.NotFound => HttpOrderStatus(Status.NotFound, message = Some("The limit order is not found"))
    case OrderStatus.PartiallyFilled(filledAmount, filledFee) => HttpOrderStatus(Status.PartiallyFilled, filledAmount.some, filledFee.some)
    case OrderStatus.Filled(filledAmount, filledFee) => HttpOrderStatus(Status.Filled, filledAmount.some, filledFee.some)
    case OrderStatus.Cancelled(filledAmount, filledFee) => HttpOrderStatus(Status.Cancelled, filledAmount.some, filledFee.some)
  }

}
