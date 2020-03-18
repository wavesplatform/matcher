package com.wavesplatform.dex.db

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

object EmptyOrderDB extends OrderDB {

  override def containsInfo(id: Order.Id): Boolean                                                  = false
  override def status(id: Order.Id): OrderStatus.Final                                              = OrderStatus.NotFound
  override def get(id: Order.Id): Option[Order]                                                     = None
  override def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit = {}
  override def saveOrder(o: Order): Unit                                                            = {}
  override def getFinalizedOrders(owner: Address, maybePair: Option[AssetPair]): Seq[(Order.Id, OrderInfo[OrderStatus])] =
    Seq.empty
}
