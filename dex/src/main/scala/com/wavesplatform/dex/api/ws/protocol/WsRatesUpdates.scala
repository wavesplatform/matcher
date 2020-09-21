package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsRatesUpdates(rates: Map[Asset, Double], updateId: Long, timestamp: Long) extends WsServerMessage {
  override def tpe: String = WsRatesUpdates.tpe
}

object WsRatesUpdates {

  val tpe = "ru"

  def wsUnapply(arg: WsRatesUpdates): Option[(String, Long, Long, Map[Asset, Double])] = (arg.tpe, arg.timestamp, arg.updateId, arg.rates).some

  implicit val wsRatesUpdatesFormat: Format[WsRatesUpdates] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "U").format[Long] and
      (__ \ "R").format[Map[Asset, Double]](json.assetMapFormat(json.stringAsDoubleFormat))
  )(
    (_, timestamp, updateId, rates) => WsRatesUpdates(rates, updateId, timestamp),
    unlift(WsRatesUpdates.wsUnapply)
  )

  /** Creates WsRatesUpdates stub, which will be enriched by update id and timestamp further in the external client handler actor */
  def broadcastUpdates(rates: Map[Asset, Double]): WsRatesUpdates = WsRatesUpdates(rates, 0, 0)
}
