package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder, WsTxsData}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsAddressChanges(
  address: Address,
  balances: Map[Asset, WsBalances],
  orders: Seq[WsOrder],
  maybeNotObservedTxs: Option[WsTxsData],
  maybeNotCreatedTxs: Option[WsTxsData],
  updateId: Long,
  timestamp: Long = System.currentTimeMillis,
  isDebug: Boolean = false
) extends WsServerMessage {
  override val tpe: String = WsAddressChanges.tpe
}

object WsAddressChanges {

  val tpe = "au"

  def wsUnapply(
    arg: WsAddressChanges
  ): Option[(String, Long, Long, Address, Map[Asset, WsBalances], Seq[WsOrder], Option[WsTxsData], Option[WsTxsData], Option[Boolean])] =
    (
      arg.tpe,
      arg.timestamp,
      arg.updateId,
      arg.address,
      arg.balances,
      arg.orders,
      arg.maybeNotObservedTxs,
      arg.maybeNotCreatedTxs,
      Option(arg.isDebug).filter(_ == true)
    ).some

  implicit val balancesMapFormat: Format[Map[Asset, WsBalances]] = json.assetMapFormat[WsBalances]

  implicit val wsAddressChangesFormat: OFormat[WsAddressChanges] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "U").format[Long] and
      (__ \ "S").format[Address] and
      (__ \ "b").formatNullable[Map[Asset, WsBalances]](balancesMapFormat) and
      (__ \ "o").formatNullable[Seq[WsOrder]] and
      (__ \ "not").formatNullable[WsTxsData] and
      (__ \ "nct").formatNullable[WsTxsData] and
      (__ \ "d").formatNullable[Boolean]
  )(
    (_, ts, uid, address, maybeBalances, maybeOrders, maybeNotObservedTxs, maybeNotCreatedTxs, isDebug) =>
      WsAddressChanges(
        address = address,
        balances = maybeBalances getOrElse Map.empty,
        orders = maybeOrders getOrElse Seq.empty,
        maybeNotObservedTxs = maybeNotObservedTxs,
        maybeNotCreatedTxs = maybeNotCreatedTxs,
        updateId = uid,
        timestamp = ts,
        isDebug = isDebug.getOrElse(false)
      ),
    unlift(WsAddressChanges.wsUnapply) andThen {
      case (tpe, ts, uid, addr, bs, os, not, nct, d) =>
        (tpe, ts, uid, addr, Option(bs).filter(_.nonEmpty), Option(os).filter(_.nonEmpty), not, nct, d)
    }
  )

}
