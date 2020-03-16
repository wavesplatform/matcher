package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsAddressState(balances: Map[Asset, WsBalances], orders: Seq[WsOrder], timestamp: Long = System.currentTimeMillis)

object WsAddressState {

  val empty: WsAddressState = WsAddressState(Map(Waves -> WsBalances(0, 0)), Seq.empty)

  implicit val balancesMapFormat: Format[Map[Asset, WsBalances]] = Format(
    { // TODO use reads for Map[Asset, T]!
      case JsObject(ab) => JsSuccess(ab.toMap.map { case (a, b) => AssetPair.extractAsset(a).get -> WsBalances.format.reads(b).get })
      case _            => JsError("Cannot parse asset balances map!")
    }, { balances =>
      Json.obj(
        balances.map {
          case (a, bs) => a.toString -> Json.toJsFieldJsValueWrapper(bs)(WsBalances.writes)
        }.toSeq: _*
      )
    }
  )

  implicit val format: Format[WsAddressState] = (
    (__ \ "b").formatNullable[Map[Asset, WsBalances]](balancesMapFormat) and
      (__ \ "o").formatNullable[Seq[WsOrder]] and
      (__ \ "_").format[Long]
  )(
    (maybeBalances, maybeOrders, timestamp) => WsAddressState(maybeBalances.getOrElse(Map.empty), maybeOrders.getOrElse(Seq.empty), timestamp),
    unlift(WsAddressState.unapply) andThen { case (b, o, t) => (Option(b).filter(_.nonEmpty), Option(o).filter(_.nonEmpty), t) }
  )
}
