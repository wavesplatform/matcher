package com.wavesplatform.dex.actors.address

import alleycats.std.set.alleyCatsStdSetMonad
import cats.instances.long._
import cats.syntax.functor._
import cats.syntax.group._
import com.wavesplatform.dex.collections.{NegativeMap, NonNegativeMap, NonPositiveMap, PositiveMap}
import com.wavesplatform.dex.domain.asset.Asset
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
 * @param futureTxIds ExchangeTransactions which hasn't been registered as unconfirmed, but will be
 */
case class AddressBalance(
  regular: NonNegativeMap[Asset, Long],
  outgoingLeasing: Option[Long],
  reserved: PositiveMap[Asset, Long],
  unconfirmed: NonPositiveMap[Asset, Long],
  notObservedTxs: Map[ExchangeTransaction.Id, NegativeMap[Asset, Long]],
  futureTxIds: Set[ExchangeTransaction.Id]
) {

  // We count only regular, because openVolume is created from orders and
  //  pessimisticCorrection updated by a stream (so, this asset couldn't be fetched)
  def allAssets: Set[Asset] = regular.xs.keySet

  /**
   * Use this method only if sure, that we known all information about specified assets
   */
  def allTradableBalances: NonNegativeMap[Asset, Long] = tradableBalances(allAssets)

  def tradableBalances(assets: Set[Asset]): NonNegativeMap[Asset, Long] = NonNegativeMap(assets.fproduct(tradableBalance).toMap)
  def tradableBalance(asset: Asset): Long = math.max(0L, nodeBalanceBy(asset) - reserved.getOrElse(asset, 0L))

  def nodeBalances(assets: Set[Asset]): Map[Asset, Long] = assets.fproduct(nodeBalanceBy).toMap

  private def nodeBalanceBy(asset: Asset): Long =
    // getOrElse is allowed here, because we fetch this information at start
    regular.getOrElse(asset, 0L) -
    (if (asset == Asset.Waves) outgoingLeasing.getOrElse(0L) else 0L) +
    unconfirmed.getOrElse(asset, 0L) +
    notObservedTxs.valuesIterator.flatMap(_.collect { case (`asset`, v) => v }).sum

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

  def reserve(diff: PositiveMap[Asset, Long]): AddressBalance = copy(reserved = reserved |+| diff)

  def fixReservation(diff: NegativeMap[Asset, Long]): AddressBalance = copy(reserved = PositiveMap((reserved.xs |+| diff.xs).filter(_._2 != 0)))

  def cancelReservation(diff: PositiveMap[Asset, Long]): AddressBalance =
    copy(reserved = PositiveMap((reserved.xs |-| diff.xs).filter(_._2 != 0)))

  /**
   * @param expectedTxId Could be None if a transaction wasn't created or has been already created (see OrderEventsCoordinatorActor)
   * @param executionTotalVolumeDiff An order's executed volume diff or a sum of two. We require a negative diff,
   *                                 because reservation is decreased each time we execute an order.
   * @return (updated, affected assets)
   */
  def withExecuted(expectedTxId: Option[ExchangeTransaction.Id], executionTotalVolumeDiff: NegativeMap[Asset, Long]): (AddressBalance, Set[Asset]) = {
    val updated = fixReservation(executionTotalVolumeDiff)
    expectedTxId match {
      case None => (updated, executionTotalVolumeDiff.keySet) // Won't expect withObserved with this txId
      case Some(txId) =>
        if (notObservedTxs.contains(txId)) throw new RuntimeException(s"$txId executed twice!")
        else if (futureTxIds.contains(txId)) (updated.copy(futureTxIds = futureTxIds - txId), executionTotalVolumeDiff.keySet)
        else (
          updated.copy(notObservedTxs = notObservedTxs.updated(txId, executionTotalVolumeDiff)),
          Set.empty // Because notObservedTxs compensates updatedOpenVolume
        )
    }
  }

  /**
   * Expected to call at most once for each txId.
   * A tx could appear twice: after appending to MemPool and after confirming in a new block.
   * But OrderEventsCoordinatorActor has a deduplication logic.
   * Even this happen, we just have a hanging txId in futureTxIds, which won't affect the process, only consumes small amount of memory.
   */
  def withObserved(txId: ExchangeTransaction.Id): (AddressBalance, Set[Asset]) =
    notObservedTxs.get(txId) match {
      case Some(v) => (copy(notObservedTxs = notObservedTxs.removed(txId)), v.keySet)
      case None => (copy(futureTxIds = futureTxIds + txId), Set.empty)
    }

}

object AddressBalance {
  val empty = AddressBalance(NonNegativeMap.empty, None, PositiveMap.empty, NonPositiveMap.empty, Map.empty, Set.empty)
}
