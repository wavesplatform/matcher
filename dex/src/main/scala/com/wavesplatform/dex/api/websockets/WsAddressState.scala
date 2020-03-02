package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import play.api.libs.json._

case class WsAddressState(balances: Map[Asset, WsBalances])

object WsAddressState {

  // TODO use reads for Map[Asset, T]!
  implicit val balancesMapReads: Reads[Map[Asset, WsBalances]] = Reads {
    case JsObject(ab) => JsSuccess(ab.toMap.map { case (a, b) => AssetPair.extractAsset(a).get -> WsBalances.reads.reads(b).get })
    case _            => JsError("Can't parse asset balances map!")
  }

  implicit val reads: Reads[WsAddressState] = Reads.map[Map[Asset, WsBalances]].map { balancesAndOrders =>
    WsAddressState(
      balances = balancesAndOrders.getOrElse("b", Map.empty)
    )
  }

  implicit val writes: Writes[WsAddressState] = Writes { was =>
    lazy val balances: JsObject = Json.obj(
      was.balances.map {
        case (a, bs) => a.toString -> Json.toJsFieldJsValueWrapper(bs)(WsBalances.writes)
      }.toSeq: _*
    )

    (was.balances.nonEmpty, false) match {
      case (true, true)  => Json.obj("b" -> balances)
      case (true, false) => Json.obj("b" -> balances)
      case (false, true) => Json.obj()
      case _             => Json.obj()
    }
  }

  implicit val format: Format[WsAddressState] = Format(reads, writes)
}
