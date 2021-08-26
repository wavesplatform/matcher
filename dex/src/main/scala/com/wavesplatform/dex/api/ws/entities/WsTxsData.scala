package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.json._
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsTxsData(
  txsData: Map[ExchangeTransaction.Id, Seq[Order.Id]],
  removedTxs: Set[ExchangeTransaction.Id]
)

object WsTxsData {

  implicit val formats: Format[WsTxsData] = (
    (__ \ "+").formatNullableWithDefault[Map[ExchangeTransaction.Id, Seq[Order.Id]]](None) and
      (__ \ "-").formatNullableWithDefault[Set[ExchangeTransaction.Id]](None)
  )(
    (txsData, removedTxs) => WsTxsData(txsData.getOrElse(Map.empty), removedTxs.getOrElse(Set.empty)),
    wsTxsData => (Option(wsTxsData.txsData).filter(_.nonEmpty), Option(wsTxsData.removedTxs).filter(_.nonEmpty))
  )

}
