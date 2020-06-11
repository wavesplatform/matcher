package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.json
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsAddressChanges(address: Address,
                            balances: Map[Asset, WsBalances],
                            orders: Seq[WsOrder],
                            updateId: Long,
                            timestamp: Long = System.currentTimeMillis)
    extends WsServerMessage {
  override val tpe: String = WsAddressChanges.tpe
}

object WsAddressChanges {

  val tpe = "au"

  def wsUnapply(arg: WsAddressChanges): Option[(String, Long, Long, Address, Map[Asset, WsBalances], Seq[WsOrder])] =
    (arg.tpe, arg.timestamp, arg.updateId, arg.address, arg.balances, arg.orders).some

  implicit val balancesMapFormat: Format[Map[Asset, WsBalances]] = json.assetMapFormat[WsBalances]

  implicit val wsAddressChangesFormat: Format[WsAddressChanges] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "U").format[Long] and
      (__ \ "S").format[Address] and
      (__ \ "b").formatNullable[Map[Asset, WsBalances]](balancesMapFormat) and
      (__ \ "o").formatNullable[Seq[WsOrder]]
  )(
    (_, ts, uid, address, maybeBalances, maybeOrders) =>
      WsAddressChanges(address, maybeBalances getOrElse Map.empty, maybeOrders getOrElse Seq.empty, uid, ts),
    unlift(WsAddressChanges.wsUnapply) andThen {
      case (tpe, ts, uid, addr, bs, os) => (tpe, ts, uid, addr, Option(bs).filter(_.nonEmpty), Option(os).filter(_.nonEmpty))
    }
  )
}
