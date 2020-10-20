package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import com.wavesplatform.dex.error.MatcherError
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, _}

case class WsError(timestamp: Long, code: Int, message: String) extends WsServerMessage {
  override def tpe: String = WsError.tpe
}

object WsError {
  val tpe = "e"

  def from(error: MatcherError, timestamp: Long): WsError = WsError(
    timestamp = timestamp,
    code = error.code,
    message = error.message.text
  )

  def wsUnapply(arg: WsError): Option[(String, Long, Int, String)] = (arg.tpe, arg.timestamp, arg.code, arg.message).some

  implicit val wsErrorFormat: Format[WsError] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "c").format[Int] and
      (__ \ "m").format[String]
  )(
    (_, timestamp, code, message) => WsError(timestamp, code, message),
    unlift(WsError.wsUnapply)
  )

}
