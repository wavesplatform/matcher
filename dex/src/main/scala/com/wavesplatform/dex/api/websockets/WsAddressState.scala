package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsAddressState(address: Address,
                          balances: Map[Asset, WsBalances],
                          orders: Seq[WsOrder],
                          updateId: Long,
                          timestamp: Long = System.currentTimeMillis)
    extends WsServerMessage {
  override val tpe: String = WsAddressState.tpe
}

object WsAddressState {

  val tpe = "au"

  def wsUnapply(arg: WsAddressState): Option[(String, Long, Long, Address, Map[Asset, WsBalances], Seq[WsOrder])] =
    (arg.tpe, arg.timestamp, arg.updateId, arg.address, arg.balances, arg.orders).some

  implicit val balancesMapFormat: Format[Map[Asset, WsBalances]] = Format(
    { // TODO use reads for Map[Asset, T]!
      case JsObject(ab) => JsSuccess(ab.toMap.map { case (a, b) => AssetPair.extractAsset(a).get -> WsBalances.wsBalancesFormat.reads(b).get })
      case _            => JsError("Cannot parse asset balances map!")
    }, { balances =>
      Json.obj(
        balances.map {
          case (a, bs) => a.toString -> Json.toJsFieldJsValueWrapper(bs)(WsBalances.writes)
        }.toSeq: _*
      )
    }
  )

  implicit val wsAddressStateFormat: Format[WsAddressState] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "U").format[Long] and
      (__ \ "S").format[Address] and
      (__ \ "b").formatNullable[Map[Asset, WsBalances]](balancesMapFormat) and
      (__ \ "o").formatNullable[Seq[WsOrder]]
  )(
    (_, ts, uid, address, maybeBalances, maybeOrders) =>
      WsAddressState(address, maybeBalances getOrElse Map.empty, maybeOrders getOrElse Seq.empty, uid, ts),
    unlift(WsAddressState.wsUnapply) andThen {
      case (tpe, ts, uid, addr, bs, os) => (tpe, ts, uid, addr, Option(bs).filter(_.nonEmpty), Option(os).filter(_.nonEmpty))
    }
  )
}
