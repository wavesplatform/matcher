package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, _}

case class WsInitial(connectionId: String, timestamp: Long) extends WsServerMessage {
  override def tpe: String = WsInitial.tpe
}

object WsInitial {

  val tpe = "i"

  def wsUnapply(arg: WsInitial): Option[(String, Long, String)] = (arg.tpe, arg.timestamp, arg.connectionId).some

  implicit val wsInitialFormat: Format[WsInitial] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "i").format[String]
  )(
    (_, timestamp, connectionId) => WsInitial(connectionId, timestamp),
    unlift(WsInitial.wsUnapply)
  )

}
