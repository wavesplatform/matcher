package com.wavesplatform.dex

import akka.actor.typed.ActorRef
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}

case class AddressWsMutableState(activeWsConnections: Map[ActorRef[WsAddressState], Long],
                                 pendingWsConnections: Set[ActorRef[WsAddressState]],
                                 changedSpendableAssets: Set[Asset],
                                 changedReservableAssets: Set[Asset],
                                 ordersChanges: Map[Order.Id, WsOrder]) {

  val hasActiveConnections: Boolean = activeWsConnections.nonEmpty
  val hasChanges: Boolean           = getAllChangedAssets.nonEmpty || ordersChanges.nonEmpty

  def getAllChangedAssets: Set[Asset]  = changedSpendableAssets ++ changedReservableAssets
  def getAllOrderChanges: Seq[WsOrder] = ordersChanges.values.toSeq

  def addPendingSubscription(subscriber: ActorRef[WsAddressState]): AddressWsMutableState =
    copy(pendingWsConnections = pendingWsConnections + subscriber)

  def flushPendingConnections(): AddressWsMutableState =
    copy(activeWsConnections = activeWsConnections ++ pendingWsConnections.iterator.map(_ -> 0L), pendingWsConnections = Set.empty)

  def removeSubscription(subscriber: ActorRef[WsAddressState]): AddressWsMutableState = {
    if (activeWsConnections.size == 1) copy(activeWsConnections = Map.empty).cleanChanges()
    else copy(activeWsConnections = activeWsConnections.filterKeys(_ != subscriber))
  }

  def putReservedAssets(diff: Set[Asset]): AddressWsMutableState  = copy(changedReservableAssets = changedReservableAssets ++ diff)
  def putSpendableAssets(diff: Set[Asset]): AddressWsMutableState = copy(changedSpendableAssets = changedSpendableAssets ++ diff)

  def putOrderUpdate(id: Order.Id, update: WsOrder): AddressWsMutableState = copy(ordersChanges = ordersChanges + (id -> update))

  def putOrderStatusUpdate(id: Order.Id, newStatus: OrderStatus): AddressWsMutableState =
    putOrderUpdate(
      id = id,
      update = ordersChanges.getOrElse(id, WsOrder(id)).copy(status = newStatus.name.some)
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
          avgWeighedPrice = ao.fillingInfo.avgWeighedPrice.some.map(denormalizePrice(_, ad, pd).toDouble)
        )
    )
  }

  def sendSnapshot(balances: Map[Asset, WsBalances], orders: Seq[WsOrder]): Unit = {
    val snapshot = WsAddressState(balances, orders, 0)
    pendingWsConnections.foreach(_ ! snapshot)
  }

  def sendDiffs(balances: Map[Asset, WsBalances], orders: Seq[WsOrder]): AddressWsMutableState = copy(
    activeWsConnections = activeWsConnections.map { // dirty but one pass
      case (conn, updateId) =>
        val newUpdateId = AddressWsMutableState.getNextUpdateId(updateId)
        conn ! WsAddressState(balances, orders, newUpdateId)
        conn -> newUpdateId
    }
  )

  def cleanChanges(): AddressWsMutableState = copy(changedSpendableAssets = Set.empty, changedReservableAssets = Set.empty, ordersChanges = Map.empty)
}

object AddressWsMutableState {

  val empty: AddressWsMutableState = AddressWsMutableState(Map.empty, Set.empty, Set.empty, Set.empty, Map.empty)
  val numberMaxSafeInteger         = 9007199254740991L

  def getNextUpdateId(currentUpdateId: Long): Long = if (currentUpdateId == numberMaxSafeInteger) 1 else currentUpdateId + 1
}
