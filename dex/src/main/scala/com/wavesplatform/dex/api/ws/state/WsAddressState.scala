package com.wavesplatform.dex.api.ws.state

import akka.actor.typed.ActorRef
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}

case class WsAddressState(
  address: Address,
  activeSubscription: Map[ActorRef[WsAddressChanges], Long],
  changedAssets: Set[Asset],
  ordersChanges: Map[Order.Id, WsOrder],
  previousBalanceChanges: Map[Asset, WsBalances]
) { // TODO Probably use an ordered Map and pass it to WsAddressChanges

  val hasActiveSubscriptions: Boolean = activeSubscription.nonEmpty
  val hasChanges: Boolean = changedAssets.nonEmpty || ordersChanges.nonEmpty

  def getAllOrderChanges: Seq[WsOrder] = ordersChanges.values.toSeq

  def addSubscription(subscriber: ActorRef[WsAddressChanges], balances: Map[Asset, WsBalances], orders: Seq[WsOrder]): WsAddressState = {
    subscriber ! WsAddressChanges(address, balances, orders, 0)
    copy(activeSubscription = activeSubscription.updated(subscriber, 0))
  }

  def removeSubscription(subscriber: ActorRef[WsAddressChanges]): WsAddressState = {
    val updated = copy(activeSubscription = activeSubscription - subscriber)
    if (updated.activeSubscription.isEmpty) updated.clean().copy(previousBalanceChanges = Map.empty)
    else updated
  }

  def putChangedAssets(diff: Set[Asset]): WsAddressState = copy(changedAssets = changedAssets ++ diff)

  def putOrderUpdate(id: Order.Id, update: WsOrder): WsAddressState = copy(ordersChanges = ordersChanges + (id -> update))

  def putOrderStatusNameUpdate(id: Order.Id, newStatus: OrderStatus): WsAddressState =
    putOrderUpdate(
      id = id,
      update = ordersChanges.getOrElse(id, WsOrder(id)).copy(status = newStatus.name.some)
    )

  def putOrderFillingInfoAndStatusNameUpdate(ao: AcceptedOrder, newStatus: OrderStatus)(implicit efc: ErrorFormatterContext): WsAddressState = {

    val ad = efc.unsafeAssetDecimals(ao.order.assetPair.amountAsset)
    val pd = efc.unsafeAssetDecimals(ao.order.assetPair.priceAsset)
    val fd = efc.unsafeAssetDecimals(ao.feeAsset)

    putOrderUpdate(
      id = ao.id,
      update = ordersChanges
        .getOrElse(ao.id, WsOrder(ao.id))
        .copy(
          status = newStatus.name.some,
          filledAmount = ao.fillingInfo.filledAmount.some.map(denormalizeAmountAndFee(_, ad).toDouble),
          filledFee = ao.fillingInfo.filledFee.some.map(denormalizeAmountAndFee(_, fd).toDouble),
          avgWeighedPrice = ao.fillingInfo.avgWeighedPrice.some.map(denormalizePrice(_, ad, pd).toDouble),
          totalExecutedPriceAssets = ao.fillingInfo.totalExecutedPriceAssets.some.map(denormalizePrice(_, ad, pd).toDouble)
        )
    )
  }

  def sendDiffs(balances: Map[Asset, WsBalances], orders: Seq[WsOrder]): WsAddressState = copy(
    activeSubscription = activeSubscription.map { // dirty but one pass
      case (conn, updateId) =>
        val newUpdateId = WsAddressState.getNextUpdateId(updateId)
        conn ! WsAddressChanges(address, balances.filterNot(Function.tupled(sameAsInPrevious)), orders, newUpdateId)
        conn -> newUpdateId
    },
    previousBalanceChanges = balances
  )

  def clean(): WsAddressState = copy(changedAssets = Set.empty, ordersChanges = Map.empty)

  private def sameAsInPrevious(asset: Asset, wsBalances: WsBalances): Boolean = previousBalanceChanges.get(asset).contains(wsBalances)
}

object WsAddressState {

  def empty(address: Address): WsAddressState = WsAddressState(address, Map.empty, Set.empty, Map.empty, Map.empty)
  val numberMaxSafeInteger = 9007199254740991L

  def getNextUpdateId(currentUpdateId: Long): Long = if (currentUpdateId == numberMaxSafeInteger) 1 else currentUpdateId + 1
}
