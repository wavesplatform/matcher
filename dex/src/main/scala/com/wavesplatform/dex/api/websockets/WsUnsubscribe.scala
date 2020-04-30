package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.WsUnsubscribe.Key
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import play.api.libs.functional.syntax._
import play.api.libs.json._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

final case class WsUnsubscribe(key: Key) extends WsClientMessage {
  override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(Json.toJson(this).toString)
  override val tpe: String                             = WsUnsubscribe.tpe
}

object WsUnsubscribe {

  type Key = AssetPair :+: Address :+: CNil

  val tpe = "u"

  def wsUnapply(arg: WsUnsubscribe): Option[(String, Key)] = (arg.tpe, arg.key).some

  private val keyFormat: Format[Key] = Format(
    fjs = Reads { json =>
      json
        .asOpt[AssetPair](assetPairKeyAsStringFormat)
        .map(x => JsSuccess(Coproduct[Key](x)))
        .orElse {
          json.asOpt[Address].map(x => JsSuccess(Coproduct[Key](x)))
        }
        .getOrElse(JsError(JsPath, "Can't parse key as address or as asset pair"))
    },
    tjs = Writes {
      case Inl(x)      => Json.toJson(x)
      case Inr(Inl(x)) => Json.toJson(x)
      case Inr(Inr(_)) => throw new IllegalArgumentException("Impossibru: CNil")
    }
  )

  implicit val wsOrderBookSubscribeFormat: Format[WsUnsubscribe] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[Key](keyFormat)
  )((_, key) => WsUnsubscribe(key), unlift(WsUnsubscribe.wsUnapply))
}
