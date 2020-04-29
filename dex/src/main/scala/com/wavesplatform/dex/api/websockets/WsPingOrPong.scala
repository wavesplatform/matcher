package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsPingOrPong(timestamp: Long) extends WsMessage {
  override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(WsPingOrPong.format.writes(this).toString)
  override val tpe: String                             = "pp"
}

object WsPingOrPong {

  def wsUnapply(arg: WsPingOrPong): Option[(String, Long)] = (arg.tpe, arg.timestamp).some

  implicit val format: Format[WsPingOrPong] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long]
  )(
    (_, ts) => WsPingOrPong(ts),
    unlift(WsPingOrPong.wsUnapply) andThen { case (tpe, ts) => (tpe, ts) }
  )
}
