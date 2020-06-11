package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.api.ws.doubleAsStringFormat
import play.api.libs.json._

case class WsBalances(tradable: Double, reserved: Double)

object WsBalances {

  implicit val reads: Reads[WsBalances] = {
    Reads.Tuple2R[Double, Double](doubleAsStringFormat, doubleAsStringFormat).map { case (t, r) => WsBalances(t, r) }
  }

  implicit val writes: Writes[WsBalances] = {
    Writes.Tuple2W[Double, Double](doubleAsStringFormat, doubleAsStringFormat).contramap(wsb => wsb.tradable -> wsb.reserved)
  }

  implicit val wsBalancesFormat: Format[WsBalances] = Format(reads, writes)
}
