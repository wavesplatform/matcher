package com.wavesplatform.dex.actors.address

import alleycats.std.set.alleyCatsStdSetMonad
import cats.instances.long._
import cats.kernel.Monoid
import cats.syntax.functor._
import cats.syntax.group._
import com.wavesplatform.dex.actors.address.AddressBalance._
import com.wavesplatform.dex.collections.{NegativeMap, NonNegativeMap, NonPositiveMap, PositiveMap}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates

/**
 * We must treat a not observed transaction as a part of unconfirmed transactions until we observe it.
 *
 * This class helps to solve the case (see 3):
 * 1. We execute an order by a some amount
 * 2. We release the open volume by an executed amount
 * 3. The tradable balance becomes higher, that should be, because we haven't yet observed a transaction of OrderExecuted in UTX,
 *      thus unconfirmed is unchanged
 * 4. The transaction is observed in UTX
 * 5. Pessimistic portfolio increased (because of the unconfirmed part), thus the account gets the right tradable balance
 *
 * @param regular Receive at start
 * @param outgoingLeasing Receive at start
 * @param reserved Amount of assets those reserved for open orders
 * @param unconfirmed Includes all transactions (exchange, transfer, issue, etc.)
 * @param notObservedTxs Volume by ExchangeTransactions which haven't yet observed in streams
 * @param notCreatedTxs ExchangeTransactions which hasn't been created on this matcher, but will be
 *
 * @see /docs/order-auto-canceling.md
 */
final case class AddressBalance(
  regular: NonNegativeMap[Asset, Long],
  outgoingLeasing: Option[Long],
  reserved: PositiveMap[Asset, Long],
  unconfirmed: NonPositiveMap[Asset, Long],
  notObservedTxs: Map[ExchangeTransaction.Id, NotObservedTxData],
  notCreatedTxs: Map[ExchangeTransaction.Id, NotCreatedTxData]
) {

  // We count only regular, because openVolume is created from orders and
  //  pessimisticCorrection updated by a stream (so, this asset couldn't be fetched)
  def allAssets: Set[Asset] = regular.xs.keySet

  /**
   * Use this method only if sure, that we known all information about specified assets
   */
  def allTradableBalance: NonNegativeMap[Asset, Long] = tradableBalance(allAssets)

  def tradableBalance(assets: Set[Asset]): NonNegativeMap[Asset, Long] = NonNegativeMap(assets.fproduct(tradableBalance).toMap)
  def tradableBalance(asset: Asset): Long = math.max(0L, nodeBalanceBy(asset) - reserved.getOrElse(asset, 0L))

  def balanceForAudit(assets: Set[Asset]): Map[Asset, Long] = assets.fproduct(balanceForAudit).toMap

  def balanceForAudit(asset: Asset): Long =
    nodeBalanceBy(asset) +
    // Compensate notCreatedTxs until they are observed
    // TODO DEX-1068 Could be slow for a lot of items in Set[Asset]
    notCreatedTxs.valuesIterator.map(_.pessimisticChanges.xs.getOrElse(asset, 0L)).sum

  private def nodeBalanceBy(asset: Asset): Long =
    // getOrElse is allowed here, because we fetch this information at start
    regular.getOrElse(asset, 0L) -
    (if (asset == Asset.Waves) outgoingLeasing.getOrElse(0L) else 0L) +
    unconfirmed.getOrElse(asset, 0L) +
    notObservedTxs.valuesIterator.map(
      _.executionTotalVolumeDiff.xs.getOrElse(asset, 0L)
    ).sum // TODO DEX-1068 Could be slow for a lot of items in Set[Asset]

  def withInit(snapshot: AddressBalanceUpdates): AddressBalance =
    // The original data have a higher precedence, because we receive it from the stream
    // And it is guaranteed to be eventually consistent.
    // Otherwise we can get a stale data and replace the fresh one by this.
    copy(
      regular = NonNegativeMap(snapshot.regular ++ regular.xs),
      outgoingLeasing = outgoingLeasing.orElse(snapshot.outgoingLeasing),
      unconfirmed = NonPositiveMap(snapshot.pessimisticCorrection ++ unconfirmed.xs)
    )

  def withFresh(updates: AddressBalanceUpdates): AddressBalance =
    copy(
      regular = NonNegativeMap(regular.xs ++ updates.regular),
      outgoingLeasing = updates.outgoingLeasing.orElse(outgoingLeasing),
      unconfirmed = NonPositiveMap(unconfirmed.xs ++ updates.pessimisticCorrection)
    )

  def reserve(diff: PositiveMap[Asset, Long]): AddressBalance =
    copy(reserved = reserved |+| diff)

  def fixReservation(diff: NegativeMap[Asset, Long]): AddressBalance =
    copy(reserved = PositiveMap((reserved.xs |+| diff.xs).filter(_._2 != 0)))

  def cancelReservation(diff: PositiveMap[Asset, Long]): AddressBalance =
    copy(reserved = PositiveMap((reserved.xs |-| diff.xs).filter(_._2 != 0)))

  /**
   * @param expectedTxId Could be None if a transaction wasn't created or has been already created (see OrderEventsCoordinatorActor)
   * @return (updated, affected assets)
   */
  def withExecuted(
    expectedTxId: Option[ExchangeTransaction.Id],
    notObservedTxData: NotObservedTxData
  ): (AddressBalance, AddressBalance.Changes) = {
    val updated = fixReservation(notObservedTxData.executionTotalVolumeDiff)
    expectedTxId match {
      case None =>
        // Won't expect withObserved with this txId
        (
          updated,
          AddressBalance.Changes.empty
            .copy(
              changedAssets = notObservedTxData.executionTotalVolumeDiff.keySet
            )
        )
      case Some(txId) =>
        if (notObservedTxs.contains(txId)) throw new RuntimeException(s"$txId executed twice!")
        else if (notCreatedTxs.contains(txId))
          (
            updated.copy(notCreatedTxs = notCreatedTxs - txId),
            AddressBalance.Changes.empty
              .copy(
                changedAssets = notObservedTxData.executionTotalVolumeDiff.keySet,
                removedNotCreatedTxs = Set(txId)
              )
          )
        else (
          updated.copy(notObservedTxs = notObservedTxs.updated(txId, notObservedTxData)),
          AddressBalance.Changes.empty.copy(
            // "changedAssets" are empty because notObservedTxs compensates updatedOpenVolume
            addedNotObservedTxs = Map(txId -> notObservedTxData.orderIds)
          )
        )
    }
  }

  /**
   * Expected to call at most once for each txId.
   * A tx could appear twice: after appending to MemPool and after confirming in a new block.
   * But OrderEventsCoordinatorActor has a deduplication logic.
   * Even this happen, we just have a hanging txId in notCreatedTxs, which won't affect the process, only consumes small amount of memory.
   */
  def withObserved(
    txId: ExchangeTransaction.Id,
    notCreatedTxData: NotCreatedTxData
  ): (AddressBalance, AddressBalance.Changes) =
    notObservedTxs.get(txId) match {
      case Some(v) =>
        (
          copy(notObservedTxs = notObservedTxs.removed(txId)),
          AddressBalance.Changes.empty
            .copy(
              changedAssets = v.executionTotalVolumeDiff.keySet,
              removedNotObservedTxs = Set(txId)
            )
        )
      case None => (
          copy(notCreatedTxs = notCreatedTxs.updated(txId, notCreatedTxData)),
          AddressBalance.Changes.empty
            .copy(
              addedNotCreatedTxs = Map(txId -> notCreatedTxData.orderIds)
            )
        )
    }

}

object AddressBalance {

  /**
   * @param executionTotalVolumeDiff An order's executed volume diff or a sum of two. We require a negative diff,
   *                                 because reservation is decreased each time we execute an order.
   */
  final case class NotObservedTxData(orderIds: Seq[Order.Id], executionTotalVolumeDiff: NegativeMap[Asset, Long])

  final case class NotCreatedTxData(orderIds: Seq[Order.Id], pessimisticChanges: PositiveMap[Asset, Long])

  final case class Changes(
    changedAssets: Set[Asset],
    addedNotObservedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    removedNotObservedTxs: Set[ExchangeTransaction.Id],
    addedNotCreatedTxs: Map[ExchangeTransaction.Id, Seq[Order.Id]],
    removedNotCreatedTxs: Set[ExchangeTransaction.Id]
  )

  object Changes {
    val empty: Changes = Changes(Set.empty, Map.empty, Set.empty, Map.empty, Set.empty)

    implicit val monoid: Monoid[Changes] = new Monoid[Changes] {
      override def empty: Changes = Changes.empty

      override def combine(x: Changes, y: Changes): Changes =
        Changes(
          x.changedAssets ++ y.changedAssets,
          x.addedNotObservedTxs ++ y.addedNotObservedTxs,
          x.removedNotObservedTxs ++ y.removedNotObservedTxs,
          x.addedNotCreatedTxs ++ y.addedNotCreatedTxs,
          x.removedNotCreatedTxs ++ y.removedNotCreatedTxs
        )

    }

  }

  val empty: AddressBalance = AddressBalance(NonNegativeMap.empty, None, PositiveMap.empty, NonPositiveMap.empty, Map.empty, Map.empty)
}
