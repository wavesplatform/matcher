package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsPingOrPong(timestamp: Long) extends WsClientMessage with WsServerMessage {
  override val tpe: String = WsPingOrPong.tpe
}

object WsPingOrPong {

  val tpe = "pp"

  def wsUnapply(arg: WsPingOrPong): Option[(String, Long)] = (arg.tpe, arg.timestamp).some

  implicit val wsPingOrPongFormat: Format[WsPingOrPong] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long]
  )(
    (_, ts) => WsPingOrPong(ts),
    unlift(WsPingOrPong.wsUnapply)
  )
}
