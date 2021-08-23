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
  notObservedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
  removedNotObservedTxs: Set[ExchangeTransaction.Id],
  notCreatedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
  removedNotCreatedTxs: Set[ExchangeTransaction.Id]
) { // TODO Probably use an ordered Map and pass it to WsAddressChanges

  val hasActiveSubscriptions: Boolean = activeSubscription.nonEmpty
  val hasChanges: Boolean = changedAssets.nonEmpty || ordersChanges.nonEmpty

  def getAllOrderChanges: Seq[WsOrder] = ordersChanges.values.toSeq

  def addSubscription(
    subscriber: ActorRef[WsAddressChanges],
    assetInfo: Map[Asset, WsAssetInfo],
    orders: Seq[WsOrder],
    flags: Set[WsAddressFlag]
  ): WsAddressState = {
    val balances = mkBalancesMap(assetInfo.filter {
      case (_: Asset, info) => nftFilteringPredicate(info, flags)
    })
    val (maybeNotObservedTxsData, maybeNotCreatedTxsData) = mkImaginaryTxsData(flags, isSnapshot = true)
    subscriber ! WsAddressChanges(address, balances, orders, maybeNotObservedTxsData, maybeNotCreatedTxsData, 0)
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
    notObservedTxsDiff: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    notCreatedTxsDiff: Map[ExchangeTransaction.Id, Seq[Order.Id]]
  ): WsAddressState = {
    val removedNotObservedTxsDiff = notObservedTxs.keySet -- notObservedTxsDiff.keySet
    val removedNotCreatedTxsDiff = notCreatedTxs.keySet -- notCreatedTxsDiff.keySet
    copy(
      notObservedTxs = notObservedTxs,
      removedNotObservedTxs = removedNotObservedTxs ++ removedNotObservedTxsDiff,
      notCreatedTxs = notCreatedTxs,
      removedNotCreatedTxs = removedNotCreatedTxs ++ removedNotCreatedTxsDiff
    )
  }

  def putOrderUpdate(id: Order.Id, update: WsOrder): WsAddressState = copy(ordersChanges = ordersChanges + (id -> update))

  def putOrderStatusNameUpdate(id: Order.Id, newStatus: OrderStatus): WsAddressState =
    putOrderUpdate(
      id = id,
      update = ordersChanges.getOrElse(id, WsOrder(id)).copy(status = newStatus.name.some)
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

    val prevChange = ordersChanges.getOrElse(ao.id, WsOrder(ao.id))
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
        val (maybeNotObservedTxsData, maybeNotCreatedTxsData) = mkImaginaryTxsData(subscription.flags, isSnapshot = false)
        conn ! WsAddressChanges(address, mkBalancesMap(preparedAssetInfo), orders, maybeNotObservedTxsData, maybeNotCreatedTxsData, newUpdateId)
        conn -> subscription.copy(updateId = newUpdateId)
    },
    previousBalanceChanges = mkBalancesMap(assetInfo)
  )

  def clean(): WsAddressState = copy(
    changedAssets = Set.empty,
    ordersChanges = Map.empty,
    removedNotCreatedTxs = Set.empty,
    removedNotObservedTxs = Set.empty
  )

  private def nftFilteringPredicate(assetInfo: WsAssetInfo, flags: Set[WsAddressFlag]): Boolean =
    if (flags.contains(WsAddressFlag.ExcludeNft))
      !assetInfo.isNft
    else true

  private def mkImaginaryTxsData(flags: Set[WsAddressFlag], isSnapshot: Boolean): (Option[WsTxsData], Option[WsTxsData]) =
    if (flags.contains(WsAddressFlag.ImaginaryTxs) && isSnapshot)
      (Some(WsTxsData(notObservedTxs, Set.empty)), Some(WsTxsData(notCreatedTxs, Set.empty)))
    else if (flags.contains(WsAddressFlag.ImaginaryTxs))
      (Some(WsTxsData(notObservedTxs, removedNotObservedTxs)), Some(WsTxsData(notCreatedTxs, removedNotCreatedTxs)))
    else
      (None, None)

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
