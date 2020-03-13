package com.wavesplatform.dex

import akka.actor.ActorRef
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.WsOrder
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}

import scala.collection.immutable.Queue

case class AddressWsMutableState(activeWsConnections: Queue[ActorRef],
                                 pendingWsConnections: Queue[ActorRef],
                                 changedSpendableAssets: Set[Asset],
                                 changedReservableAssets: Set[Asset],
                                 ordersChanges: Map[Order.Id, WsOrder],
                                 trackedOrders: Set[Order.Id]) {

  val hasActiveConnections: Boolean = activeWsConnections.nonEmpty
  val hasChangedAssets: Boolean     = getAllChangedAssets.nonEmpty

  def getAllChangedAssets: Set[Asset]  = changedSpendableAssets ++ changedReservableAssets
  def getAllOrderChanges: Seq[WsOrder] = ordersChanges.values.toSeq

  def addPendingSubscription(subscriber: ActorRef): AddressWsMutableState = copy(pendingWsConnections = pendingWsConnections enqueue subscriber)

  def flushPendingConnections(): AddressWsMutableState =
    copy(activeWsConnections = activeWsConnections ++ pendingWsConnections, pendingWsConnections = Queue.empty)

  def removeSubscription(subscriber: ActorRef): AddressWsMutableState = {
    if (activeWsConnections.lengthCompare(1) == 0)
      copy(activeWsConnections = Queue.empty,
           changedReservableAssets = Set.empty,
           changedSpendableAssets = Set.empty,
           ordersChanges = Map.empty,
           trackedOrders = Set.empty)
    else copy(activeWsConnections = activeWsConnections.filterNot(_ == subscriber))
  }

  def putReservedAssets(diff: Set[Asset]): AddressWsMutableState  = copy(changedReservableAssets = changedReservableAssets ++ diff)
  def putSpendableAssets(diff: Set[Asset]): AddressWsMutableState = copy(changedSpendableAssets = changedSpendableAssets ++ diff)

  def putOrderUpdate(id: Order.Id, update: WsOrder): AddressWsMutableState =
    copy(ordersChanges = ordersChanges + (id -> update), trackedOrders = trackedOrders + id)

  def putOrderStatusUpdate(id: Order.Id, newStatus: OrderStatus): AddressWsMutableState =
    putOrderUpdate(
      id = id,
      update = ordersChanges.getOrElse(id, WsOrder(id)).copy(status = newStatus.name.some)
    ).copy(
      trackedOrders = newStatus match {
        case _: OrderStatus.Final => trackedOrders - id
        case _                    => trackedOrders
      }
    )

  def putOrderFillingInfoAndStatusUpdate(ao: AcceptedOrder, newStatus: OrderStatus)(implicit efc: ErrorFormatterContext): AddressWsMutableState = {

    val ad = efc.assetDecimals(ao.order.assetPair.amountAsset)
    val pd = efc.assetDecimals(ao.order.assetPair.priceAsset)

    putOrderUpdate(
      id = ao.id,
      update = ordersChanges
        .getOrElse(ao.id, WsOrder(ao.id))
        .copy(
          status = newStatus.name.some,
          filledAmount = ao.fillingInfo.filledAmount.some.map(denormalizeAmountAndFee(_, ad).toDouble),
          filledFee = ao.fillingInfo.filledFee.some.map(denormalizeAmountAndFee(_, ad).toDouble),
          avgFilledPrice = ao.fillingInfo.avgFilledPrice.some.map(denormalizePrice(_, ad, pd).toDouble)
        )
    )
  }

  def cleanChanges(): AddressWsMutableState = copy(changedSpendableAssets = Set.empty, changedReservableAssets = Set.empty, ordersChanges = Map.empty)
}

object AddressWsMutableState {
  val empty: AddressWsMutableState = AddressWsMutableState(Queue.empty, Queue.empty, Set.empty, Set.empty, Map.empty, Set.empty)
}
