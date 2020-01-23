package com.wavesplatform.dex.db

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

class TestOrderDB(maxOrdersPerRequest: Int) extends OrderDB {

  private var knownOrders   = Map.empty[Order.Id, Order]
  private var orderInfo     = Map.empty[Order.Id, OrderInfo[OrderStatus.Final]]
  private var idsForPair    = Map.empty[(Address, AssetPair), Seq[Order.Id]].withDefaultValue(Seq.empty)
  private var idsForAddress = Map.empty[Address, Seq[Order.Id]].withDefaultValue(Seq.empty)

  override def containsInfo(id: Order.Id): Boolean = orderInfo.contains(id)

  override def get(id: Order.Id): Option[Order] = knownOrders.get(id)

  override def status(id: Order.Id): OrderStatus.Final = orderInfo.get(id).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)

  override def saveOrderInfo(id: Order.Id, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit = if (!containsInfo(id)) {
    orderInfo += id                      -> oi
    idsForAddress += sender              -> (id +: idsForAddress(sender))
    idsForPair += (sender, oi.assetPair) -> (id +: idsForPair(sender -> oi.assetPair))
  }

  override def saveOrder(o: Order): Unit = knownOrders += o.id() -> o

  override def loadRemainingOrders(owner: Address,
                                   maybePair: Option[AssetPair],
                                   activeOrders: Seq[(Order.Id, OrderInfo[OrderStatus])]): Seq[(Order.Id, OrderInfo[OrderStatus])] =
    activeOrders ++ (for {
      id   <- maybePair.fold(idsForAddress(owner))(p => idsForPair(owner -> p))
      info <- orderInfo.get(id)
    } yield id -> info)
      .sortBy { case (_, oi) => -oi.timestamp }
      .take(maxOrdersPerRequest - activeOrders.length)
}
