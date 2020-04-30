package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsPingOrPong(timestamp: Long) extends WsClientMessage {
  override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(Json.toJson(this).toString)
  override val tpe: String                             = WsPingOrPong.tpe
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
