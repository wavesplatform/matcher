package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.json._
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsTxsData(
  txsData: Option[Map[ExchangeTransaction.Id, Seq[Order.Id]]],
  removedTxs: Option[Set[ExchangeTransaction.Id]]
)

object WsTxsData {

  implicit private def byteStr58MapFormat[V: Format]: Format[Map[ByteStr, V]] = mapFormat[ByteStr, V](
    stringifyKey = _.base58,
    parseKey = x =>
      ByteStr.decodeBase58(x).fold[JsResult[ByteStr]](
        err => JsError(s"Can't parse '$x' as ByteStr, expected base58 string, error $err"),
        JsSuccess(_)
      )
  )

  implicit val formats: Format[WsTxsData] = (
    (__ \ "+").formatNullable[Map[ExchangeTransaction.Id, Seq[Order.Id]]] and
      (__ \ "-").formatNullable[Set[ExchangeTransaction.Id]]
  )(WsTxsData.apply, unlift(WsTxsData.unapply))

}
