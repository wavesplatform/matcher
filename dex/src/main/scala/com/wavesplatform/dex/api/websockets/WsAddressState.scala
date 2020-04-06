package com.wavesplatform.dex.api.websockets

import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsAddressState(balances: Map[Asset, WsBalances], orders: Seq[WsOrder], updateId: Long, timestamp: Long = System.currentTimeMillis)
    extends WsMessage {
  override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(WsAddressState.format.writes(this).toString)
  override val tpe: String                             = "au"
}

object WsAddressState {

  def wsUnapply(arg: WsAddressState): Option[(String, Long, Long, Map[Asset, WsBalances], Seq[WsOrder])] =
    (arg.tpe, arg.timestamp, arg.updateId, arg.balances, arg.orders).some

  val empty: WsAddressState = WsAddressState(Map(Waves -> WsBalances(0, 0)), Seq.empty, 0)

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
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "U").format[Long] and
      (__ \ "b").formatNullable[Map[Asset, WsBalances]](balancesMapFormat) and
      (__ \ "o").formatNullable[Seq[WsOrder]]
  )(
    (_, ts, uid, maybeBalances, maybeOrders) => WsAddressState(maybeBalances getOrElse Map.empty, maybeOrders getOrElse Seq.empty, uid, ts),
    unlift(WsAddressState.wsUnapply) andThen {
      case (tpe, ts, uid, bs, os) => (tpe, ts, uid, Option(bs).filter(_.nonEmpty), Option(os).filter(_.nonEmpty))
    }
  )
}
