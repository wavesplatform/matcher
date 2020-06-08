package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrdersUpdate(orders: List[WsCompleteOrder], timestamp: Long = System.currentTimeMillis) extends WsServerMessage {
  override val tpe: String = WsOrdersUpdate.tpe
}

object WsOrdersUpdate {

  val tpe = "osu"

  def wsUnapply(arg: WsOrdersUpdate): Option[(String, Long, List[WsCompleteOrder])] = (arg.tpe, arg.timestamp, arg.orders).some

  implicit val wsOrdersUpdateFormat: Format[WsOrdersUpdate] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "o").format[List[WsCompleteOrder]]
  )(
    (_, ts, maybeOrders) => WsOrdersUpdate(maybeOrders, ts),
    unlift(WsOrdersUpdate.wsUnapply)
  )
}
