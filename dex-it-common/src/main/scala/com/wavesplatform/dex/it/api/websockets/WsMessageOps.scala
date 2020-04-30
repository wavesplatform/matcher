package com.wavesplatform.dex.it.api.websockets

import com.wavesplatform.dex.api.websockets.{WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.Order

trait WsMessageOps {
  implicit class WsBalancesChangesListOps(val self: List[Map[Asset, WsBalances]]) {
    def squashed: Map[Asset, WsBalances] = self.foldLeft(Map.empty[Asset, WsBalances])(_ ++ _)
  }

  implicit class WsOrderChangesListOps(val self: List[WsOrder]) {
    def squashed: Map[Order.Id, WsOrder] =
      self
        .groupBy(_.id)
        .map {
          case (id, orderChanges) =>
            id -> orderChanges.foldLeft(orderChanges.head) {
              case (acc, oc) =>
                acc.copy(status = oc.status, filledAmount = oc.filledAmount, filledFee = oc.filledFee, avgWeighedPrice = oc.avgWeighedPrice)
            }
        }
  }
}
