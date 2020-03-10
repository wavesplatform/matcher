package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsAddressState(balances: Map[Asset, WsBalances], orders: Seq[WsOrder])

object WsAddressState {

  implicit val balancesMapFormat: Format[Map[Asset, WsBalances]] = Format(
    { // TODO use reads for Map[Asset, T]!
      case JsObject(ab) => JsSuccess(ab.toMap.map { case (a, b) => AssetPair.extractAsset(a).get -> WsBalances.reads.reads(b).get })
      case _            => JsError("Cannot parse asset balances map!")
    }, { balances =>
      Json.obj(
        balances.map {
          case (a, bs) => a.toString -> Json.toJsFieldJsValueWrapper(bs)(WsBalances.writes)
        }.toSeq: _*
      )
    }
  )

  implicit val writes: Writes[WsAddressState] =
    (
      (JsPath \ "b").writeNullable[Map[Asset, WsBalances]](balancesMapFormat.writes) and
        (JsPath \ "o").writeNullable[Seq[WsOrder]]
    )(unlift(WsAddressState.unapply) andThen { case (b, o) => Option(b).filter(_.nonEmpty) -> Option(o).filter(_.nonEmpty) })

  implicit val reads: Reads[WsAddressState] =
    (
      (JsPath \ "b").readNullable[Map[Asset, WsBalances]](balancesMapFormat.reads) and
        (JsPath \ "o").readNullable[Seq[WsOrder]]
    ) { (maybeBalances, maybeOrders) =>
      WsAddressState(maybeBalances.getOrElse(Map.empty), maybeOrders.getOrElse(Seq.empty))
    }

  implicit val format: Format[WsAddressState] = Format(reads, writes)
}
