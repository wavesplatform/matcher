package com.wavesplatform.dex.api.ws.protocol

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsRatesUpdatesSubscribe(id: String = "ru") extends WsClientMessage {
  override def tpe: String = WsRatesUpdatesSubscribe.tpe
}

object WsRatesUpdatesSubscribe {

  val tpe = "rus"
  val subscribeId = "ru"

  implicit val wsRatesUpdatesSubscribeFormat: Format[WsRatesUpdatesSubscribe] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[String]
  )(
    (_, id) => WsRatesUpdatesSubscribe(id),
    unlift(s => Option(tpe -> s.id))
  )

  def validateId(wsRatesUpdatesSubscribe: WsRatesUpdatesSubscribe): JsResult[WsRatesUpdatesSubscribe] =
    if (wsRatesUpdatesSubscribe.id == subscribeId) JsSuccess(wsRatesUpdatesSubscribe)
    else JsError(JsPath \ "S", s"Unexpected subscribe id, got: ${wsRatesUpdatesSubscribe.id}, but expected $subscribeId")

}
