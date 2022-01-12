package com.wavesplatform.dex.api.ws.protocol

import cats.data.NonEmptyList
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.WsFullOrder
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrdersUpdate(orders: NonEmptyList[WsFullOrder], timestamp: Long = System.currentTimeMillis) extends WsServerMessage {
  override val tpe: String = WsOrdersUpdate.tpe

  def append(other: WsOrdersUpdate): WsOrdersUpdate = copy(
    orders = other.orders ::: orders,
    timestamp = other.timestamp
  )

}

object WsOrdersUpdate {

  val tpe = "osu"

  def wsUnapply(arg: WsOrdersUpdate): Option[(String, Long, NonEmptyList[WsFullOrder])] = (arg.tpe, arg.timestamp, arg.orders).some

  implicit def nonEmptyListFormat[T: Format]: Format[NonEmptyList[T]] = Format(
    Reads.list[T].flatMap { xs =>
      NonEmptyList.fromList(xs).fold[Reads[NonEmptyList[T]]](Reads.failed("The list is empty"))(Reads.pure(_))
    },
    Writes.list[T].contramap(_.toList)
  )

  implicit val wsOrdersUpdateFormat: Format[WsOrdersUpdate] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "o").format(nonEmptyListFormat[WsFullOrder])
  )(
    (_, ts, orders) => WsOrdersUpdate(orders, ts),
    unlift(WsOrdersUpdate.wsUnapply)
  )

}
