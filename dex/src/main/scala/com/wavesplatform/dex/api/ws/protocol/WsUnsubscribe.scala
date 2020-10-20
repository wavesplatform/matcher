package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import com.wavesplatform.dex.api.ws.protocol.WsUnsubscribe.Key
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import play.api.libs.functional.syntax._
import play.api.libs.json._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

final case class WsUnsubscribe(key: Key) extends WsClientMessage {
  override val tpe: String = WsUnsubscribe.tpe
}

object WsUnsubscribe {

  type Key = AssetPair :+: Address :+: String :+: CNil

  val tpe = "u"

  def apply(key: AssetPair): WsUnsubscribe = new WsUnsubscribe(Coproduct[Key](key))
  def apply(key: Address): WsUnsubscribe = new WsUnsubscribe(Coproduct[Key](key))
  def apply(key: String): WsUnsubscribe = new WsUnsubscribe(Coproduct[Key](key))

  def wsUnapply(arg: WsUnsubscribe): Option[(String, Key)] = (arg.tpe, arg.key).some

  private val keyFormat: Format[Key] = Format(
    fjs = Reads { json =>
      json
        .asOpt[AssetPair](AssetPair.assetPairKeyAsStringFormat)
        .map(x => JsSuccess(Coproduct[Key](x)))
        .orElse(json.asOpt[Address].map(x => JsSuccess(Coproduct[Key](x))))
        .orElse {
          json
            .asOpt[String]
            .map(x =>
              if (x == WsRatesUpdatesSubscribe.subscribeId) JsSuccess(Coproduct[Key](x))
              else JsError(JsPath \ "S", s"Unexpected subscribe id, got: $x, but expected ${WsRatesUpdatesSubscribe.subscribeId}")
            )
        }
        .getOrElse(JsError(JsPath, "Can't parse key as address or as asset pair"))
    },
    tjs = Writes {
      case Inl(x) => Json.toJson(x)(AssetPair.assetPairKeyAsStringFormat) // asset pair
      case Inr(Inl(x)) => Json.toJson(x) // address
      case Inr(Inr(Inl(x))) => Json.toJson(x) // rates update "ru" constant
      case Inr(Inr(_)) => throw new IllegalArgumentException("Impossibru: CNil")
    }
  )

  implicit val wsOrderBookSubscribeFormat: Format[WsUnsubscribe] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[Key](keyFormat)
  )((_, key) => WsUnsubscribe(key), unlift(WsUnsubscribe.wsUnapply))

}
