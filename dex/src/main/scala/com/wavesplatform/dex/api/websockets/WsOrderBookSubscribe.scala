package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.AssetPair
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsOrderBookSubscribe(key: AssetPair, depth: Int) extends WsClientMessage {
  override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(Json.toJson(this).toString)
  override val tpe: String                             = WsOrderBookSubscribe.tpe
}

object WsOrderBookSubscribe {

  val tpe = "obs"

  def wsUnapply(arg: WsOrderBookSubscribe): Option[(String, AssetPair, Int)] = (arg.tpe, arg.key, arg.depth).some

  implicit val wsOrderBookSubscribeFormat: Format[WsOrderBookSubscribe] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[AssetPair](assetPairKeyAsStringFormat) and
      (__ \ "d").format[Int]
  )(
    (_, key, depth) => WsOrderBookSubscribe(key, depth),
    unlift(WsOrderBookSubscribe.wsUnapply)
  )
}
