package com.wavesplatform.dex.api.ws.state

import akka.actor.typed.ActorRef
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities._
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.api.ws.state.WsAddressState.Subscription
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}

final case class WsAddressState(
  address: Address,
  activeSubscription: Map[ActorRef[WsAddressChanges], Subscription],
  changedAssets: Set[Asset],
  ordersChanges: Map[Order.Id, WsOrder],
  previousBalanceChanges: Map[Asset, WsBalances],
  addedNotObservedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
  removedNotObservedTxs: Set[ExchangeTransaction.Id],
  addedNotCreatedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
  removedNotCreatedTxs: Set[ExchangeTransaction.Id]
) { // TODO Probably use an ordered Map and pass it to WsAddressChanges

  val hasActiveSubscriptions: Boolean = activeSubscription.nonEmpty
  val hasChanges: Boolean = changedAssets.nonEmpty || ordersChanges.nonEmpty

  def getAllOrderChanges: Seq[WsOrder] = ordersChanges.values.toSeq

  def addSubscription(
    subscriber: ActorRef[WsAddressChanges],
    assetInfo: Map[Asset, WsAssetInfo],
    orders: Seq[WsOrder],
    notObservedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    notCreatedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    flags: Set[WsAddressFlag],
    isDebug: Boolean = false
  ): WsAddressState = {
    val balances = mkBalancesMap(assetInfo.filter {
      case (_: Asset, info) => nftFilteringPredicate(info, flags)
    })
    val (maybeNotObservedTxsData, maybeNotCreatedTxsData) =
      mkImaginaryTxsData(notObservedTxs, Set.empty, notCreatedTxs, Set.empty, flags)
    subscriber ! WsAddressChanges(address, balances, orders, maybeNotObservedTxsData, maybeNotCreatedTxsData, 0, isDebug = isDebug)
    copy(activeSubscription = activeSubscription.updated(subscriber, Subscription(0, flags)))
  }

  def removeSubscription(subscriber: ActorRef[WsAddressChanges]): WsAddressState = {
    val updated = copy(activeSubscription = activeSubscription - subscriber)
    if (updated.activeSubscription.isEmpty) updated.clean().copy(previousBalanceChanges = Map.empty)
    else updated
  }

  def putChangedAssets(diff: Set[Asset]): WsAddressState =
    copy(changedAssets = changedAssets ++ diff)

  def putTxsUpdate(
    addedNotObservedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    removedNotObservedTxs: Set[ExchangeTransaction.Id],
    addedNotCreatedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    removedNotCreatedTxs: Set[ExchangeTransaction.Id]
  ): WsAddressState =
    copy(
      addedNotObservedTxs = this.addedNotObservedTxs ++ addedNotObservedTxs,
      removedNotObservedTxs = this.removedNotObservedTxs ++ removedNotObservedTxs,
      addedNotCreatedTxs = this.addedNotCreatedTxs ++ addedNotCreatedTxs,
      removedNotCreatedTxs = this.removedNotCreatedTxs ++ removedNotCreatedTxs
    )

  def putOrderUpdate(id: Order.Id, update: WsOrder): WsAddressState = copy(ordersChanges = ordersChanges + (id -> update))

  def putOrderStatusNameUpdate(order: Order, newStatus: OrderStatus): WsAddressState =
    putOrderUpdate(
      id = order.id(),
      update = ordersChanges.getOrElse(order.id(), WsOrder.fromOrder(order)).copy(status = newStatus.name.some)
    )

  def putOrderFillingInfoAndStatusNameUpdate(
    ao: AcceptedOrder,
    newStatus: OrderStatus,
    maybeMatchTx: Option[ExchangeTransaction] = None
  )(implicit efc: ErrorFormatterContext): WsAddressState = {
    val ad = efc.unsafeAssetDecimals(ao.order.assetPair.amountAsset)
    val pd = efc.unsafeAssetDecimals(ao.order.assetPair.priceAsset)
    val fd = efc.unsafeAssetDecimals(ao.feeAsset)

    def mkMatchTxInfo(): Option[WsMatchTransactionInfo] = maybeMatchTx.map { matchTx =>
      WsMatchTransactionInfo.normalized(
        ao.order.assetPair,
        txId = matchTx.id(),
        timestamp = matchTx.timestamp,
        price = matchTx.price,
        executedAmountAssets = matchTx.amount
      )
    }

    val prevChange = ordersChanges.getOrElse(ao.id, WsOrder.fromOrder(ao.order))
    putOrderUpdate(
      id = ao.id,
      update = prevChange.copy(
        status = newStatus.name.some,
        filledAmount = ao.fillingInfo.filledAmount.some.map(denormalizeAmountAndFee(_, ad).toDouble),
        filledFee = ao.fillingInfo.filledFee.some.map(denormalizeAmountAndFee(_, fd).toDouble),
        avgWeighedPrice = ao.fillingInfo.avgWeighedPrice.some.map(denormalizePrice(_, ad, pd).toDouble),
        totalExecutedPriceAssets = ao.fillingInfo.totalExecutedPriceAssets.some.map(denormalizePrice(_, ad, pd).toDouble),
        matchInfo = mkMatchTxInfo().fold(prevChange.matchInfo)(newMatchInfo => prevChange.matchInfo :+ newMatchInfo)
      )
    )
  }

  def sendDiffs(assetInfo: Map[Asset, WsAssetInfo], orders: Seq[WsOrder]): WsAddressState = copy(
    activeSubscription = activeSubscription.map { // dirty but one pass
      case (conn, subscription) =>
        val newUpdateId = WsAddressState.getNextUpdateId(subscription.updateId)
        val preparedAssetInfo = assetInfo
          .filter { case (asset, info) =>
            !sameAsInPrevious(asset, info.balances) && nftFilteringPredicate(info, subscription.flags)
          }
        val (maybeNotObservedTxsData, maybeNotCreatedTxsData) =
          mkImaginaryTxsData(addedNotObservedTxs, removedNotObservedTxs, addedNotCreatedTxs, removedNotCreatedTxs, subscription.flags)
        conn ! WsAddressChanges(address, mkBalancesMap(preparedAssetInfo), orders, maybeNotObservedTxsData, maybeNotCreatedTxsData, newUpdateId)
        conn -> subscription.copy(updateId = newUpdateId)
    },
    previousBalanceChanges = mkBalancesMap(assetInfo)
  )

  def clean(): WsAddressState = copy(
    changedAssets = Set.empty,
    ordersChanges = Map.empty,
    addedNotObservedTxs = Map.empty,
    removedNotObservedTxs = Set.empty,
    addedNotCreatedTxs = Map.empty,
    removedNotCreatedTxs = Set.empty
  )

  private def nftFilteringPredicate(assetInfo: WsAssetInfo, flags: Set[WsAddressFlag]): Boolean =
    if (flags.contains(WsAddressFlag.ExcludeNft))
      !assetInfo.isNft
    else true

  private def mkImaginaryTxsData(
    notObservedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    removedNotObservedTxs: Set[ExchangeTransaction.Id],
    notCreatedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    removedNotCreatedTxs: Set[ExchangeTransaction.Id],
    flags: Set[WsAddressFlag]
  ): (Option[WsTxsData], Option[WsTxsData]) = {
    def mkMaybeWsTxsData(txsData: Map[ExchangeTransaction.Id, Seq[Order.Id]], removed: Set[ExchangeTransaction.Id]): Option[WsTxsData] = {
      val wsTxsData =
        if (flags.contains(WsAddressFlag.ImaginaryTxs))
          WsTxsData(txsData, removed)
        else
          WsTxsData(Map.empty, Set.empty)

      Option(wsTxsData).filter(x => x.txsData.nonEmpty || x.removedTxs.nonEmpty)
    }

    (mkMaybeWsTxsData(notObservedTxs, removedNotObservedTxs), mkMaybeWsTxsData(notCreatedTxs, removedNotCreatedTxs))
  }

  private def sameAsInPrevious(asset: Asset, wsBalances: WsBalances): Boolean = previousBalanceChanges.get(asset).contains(wsBalances)

  private def mkBalancesMap(assetInfo: Map[Asset, WsAssetInfo]): Map[Asset, WsBalances] = assetInfo.map { case (asset, info) =>
    (asset, info.balances)
  }

}

object WsAddressState {

  case class Subscription(updateId: Long, flags: Set[WsAddressFlag])

  def empty(address: Address): WsAddressState =
    WsAddressState(address, Map.empty, Set.empty, Map.empty, Map.empty, Map.empty, Set.empty, Map.empty, Set.empty)

  val numberMaxSafeInteger = 9007199254740991L

  def getNextUpdateId(currentUpdateId: Long): Long = if (currentUpdateId == numberMaxSafeInteger) 1 else currentUpdateId + 1
}
