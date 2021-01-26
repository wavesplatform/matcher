package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.collections.{NegativeMap, NonNegativeMap, NonPositiveMap, PositiveMap}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates

/**
 * @param outgoingLeasing Receive at start
 * @param pessimisticCorrection Receive at start
 * @param openVolume Amount of assets those reserved for open orders
 */
case class AddressBalance(
  regular: NonNegativeMap[Asset, Long],
  outgoingLeasing: Option[Long],
//  pessimisticCorrection: AddressPessimisticCorrection,
  openVolume: PositiveMap[Asset, Long],
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
  def allTradableBalances: Map[Asset, Long] = tradableBalances(allAssets)

  def tradableBalances(assets: Set[Asset]): Map[Asset, Long] =
    assets.foldLeft(Map.empty[Asset, Long]) {
      case (r, asset) =>
        // getOrElse, because we fetch this information at start
        val x =
          regular.getOrElse(asset, 0L) -
          (if (asset == Asset.Waves) outgoingLeasing.getOrElse(0L) else 0L) - // !!
          openVolume.getOrElse(asset, 0L) +
          unconfirmed.getOrElse(asset, 0L) +
          notObservedTxs.valuesIterator.flatMap(_.collect { case (`asset`, v) => v }).sum

        r.updated(asset, math.max(0L, x))
    }

  def nodeBalances(assets: Set[Asset]): Map[Asset, Long] =
    assets.foldLeft(Map.empty[Asset, Long]) {
      case (r, asset) =>
        // getOrElse, because we fetch this information at start
        val x =
          regular.getOrElse(asset, 0L) -
          (if (asset == Asset.Waves) outgoingLeasing.getOrElse(0L) else 0L) + // !!
          unconfirmed.getOrElse(asset, 0L) +
          notObservedTxs.valuesIterator.flatMap(_.collect { case (`asset`, v) => v }).sum

        r.updated(asset, x)
    }

  def withInit(snapshot: AddressBalanceUpdates): AddressBalance =
    // The original data have a higher precedence, because we receive it from the stream
    // And it is guaranteed to be eventually consistent.
    // Otherwise we can get a stale data and replace the fresh one by this.
    copy(
      regular = NonNegativeMap(snapshot.regular ++ regular.xs),
      outgoingLeasing = outgoingLeasing.orElse(snapshot.outLease),
      unconfirmed = NonPositiveMap(snapshot.pessimisticCorrection ++ unconfirmed.xs)
    )

  def withFresh(updates: AddressBalanceUpdates): AddressBalance =
    copy(
      regular = NonNegativeMap(regular.xs ++ updates.regular),
      outgoingLeasing = updates.outLease.orElse(outgoingLeasing),
      unconfirmed = NonPositiveMap(unconfirmed.xs ++ updates.pessimisticCorrection)
    )

  def appendedOpenVolume(diff: Map[Asset, Long]): AddressBalance = copy(openVolume = PositiveMap((openVolume.xs |+| diff).filter(_._2 != 0)))

  def removedOpenVolume(diff: Map[Asset, Long]): AddressBalance = copy(openVolume = PositiveMap((openVolume.xs |-| diff).filter(_._2 != 0)))

  /**
   * @param executionTotalVolumeDiff An order's executed assets or a sum of two
   */
  def withExecuted(expectedTxId: Option[ExchangeTransaction.Id], executionTotalVolumeDiff: Map[Asset, Long]): (AddressBalance, Set[Asset]) = {
    val updated = copy(openVolume = PositiveMap((openVolume.xs |+| executionTotalVolumeDiff).filter(_._2 != 0)))
    expectedTxId match {
      case None => (updated, executionTotalVolumeDiff.keySet) // Won't expect withObserved with this txId
      case Some(txId) =>
        if (notObservedTxs.contains(txId)) throw new RuntimeException(s"$txId executed twice!")
        else if (futureTxIds.contains(txId)) (updated.copy(futureTxIds = futureTxIds - txId), executionTotalVolumeDiff.keySet)
        else (
          updated.copy(notObservedTxs = notObservedTxs.updated(txId, NegativeMap(executionTotalVolumeDiff))),
          Set.empty // Because notObservedTxs compensates updatedOpenVolume
        )
    }
  }

  def withObserved(txId: ExchangeTransaction.Id): (AddressBalance, Set[Asset]) =
    notObservedTxs.get(txId) match {
      case Some(v) => (copy(notObservedTxs = notObservedTxs.removed(txId)), v.keySet)
      case None => (copy(futureTxIds = futureTxIds + txId), Set.empty)
    }

}

object AddressBalance {
  val empty = AddressBalance(NonNegativeMap.empty, None, PositiveMap.empty, NonPositiveMap.empty, Map.empty, Set.empty)
}
