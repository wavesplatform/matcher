package com.wavesplatform.dex.api

import cats.syntax.option._
import com.wavesplatform.dex.api.ApiOrderStatus.Status
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.dex.util.getSimpleName
import play.api.libs.json.{Format, Json, Reads, Writes}

case class ApiOrderStatus(status: Status, filledAmount: Option[Long] = None, filledFee: Option[Long] = None, message: Option[String] = None)

object ApiOrderStatus {

  implicit val apiOrderStatusFormat: Format[ApiOrderStatus] = Json.format

  def from(x: OrderStatus): ApiOrderStatus = x match {
    case OrderStatus.Accepted                                 => ApiOrderStatus(Status.Accepted)
    case OrderStatus.NotFound                                 => ApiOrderStatus(Status.NotFound, message = Some("The limit order is not found"))
    case OrderStatus.PartiallyFilled(filledAmount, filledFee) => ApiOrderStatus(Status.PartiallyFilled, filledAmount.some, filledFee.some)
    case OrderStatus.Filled(filledAmount, filledFee)          => ApiOrderStatus(Status.Filled, filledAmount.some, filledFee.some)
    case OrderStatus.Cancelled(filledAmount, filledFee)       => ApiOrderStatus(Status.Cancelled, filledAmount.some, filledFee.some)
  }

  sealed abstract class Status extends Product with Serializable {
    val name: String = getSimpleName(this)
  }

  object Status {

    case object Accepted        extends Status
    case object NotFound        extends Status
    case object PartiallyFilled extends Status
    case object Filled          extends Status
    case object Cancelled       extends Status

    val All = List(Accepted, NotFound, PartiallyFilled, Filled, Cancelled)

    implicit val format: Format[Status] = Format(
      Reads.StringReads.map { x =>
        All.find(_.name == x) match {
          case Some(r) => r
          case None    => throw new IllegalArgumentException(s"Can't parse '$x' as ApiOrderStatus.Status")
        }
      },
      Writes.StringWrites.contramap(_.name)
    )
  }
}
