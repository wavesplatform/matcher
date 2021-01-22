package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.collections.{NonPositiveMap, PositiveMap}
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
  regular: PositiveMap[Asset, Long],
  outgoingLeasing: Option[Long],
  pessimisticCorrection: AddressPessimisticCorrection,
  openVolume: PositiveMap[Asset, Long]
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
          (if (asset == Asset.Waves) outgoingLeasing.getOrElse(0L) else 0L) + // !!
          pessimisticCorrection.getBy(asset) -
          openVolume.getOrElse(asset, 0L)

        r.updated(asset, math.max(0L, x))
    }

  def nodeBalances(assets: Set[Asset]): Map[Asset, Long] =
    assets.foldLeft(Map.empty[Asset, Long]) {
      case (r, asset) =>
        // getOrElse, because we fetch this information at start
        val x =
          regular.getOrElse(asset, 0L) -
          (if (asset == Asset.Waves) outgoingLeasing.getOrElse(0L) else 0L) + // !!
          pessimisticCorrection.getBy(asset)

        r.updated(asset, x)
    }

  def withInit(snapshot: AddressBalanceUpdates): AddressBalance =
    // The original data have a higher precedence, because we receive it from the stream
    // And it is guaranteed to be eventually consistent.
    // Otherwise we can get a stale data and replace the fresh one by it.
    AddressBalance(
      regular = PositiveMap((snapshot.regular ++ regular.xs).filter(_._2 != 0)),
      outgoingLeasing = outgoingLeasing.orElse(snapshot.outLease),
      pessimisticCorrection = pessimisticCorrection.withInit(NonPositiveMap(snapshot.pessimisticCorrection)),
      openVolume = openVolume
    )

  def withFresh(updates: AddressBalanceUpdates): AddressBalance =
    AddressBalance(
      regular = PositiveMap((regular.xs ++ updates.regular).filter(_._2 != 0)),
      outgoingLeasing = updates.outLease.orElse(outgoingLeasing),
      pessimisticCorrection = pessimisticCorrection.withFreshUnconfirmed(NonPositiveMap(updates.pessimisticCorrection)),
      openVolume = openVolume
    )

  def appendedOpenVolume(diff: Map[Asset, Long]): AddressBalance = copy(openVolume = PositiveMap((openVolume.xs |+| diff).filter(_._2 != 0)))

  def removedOpenVolume(diff: Map[Asset, Long]): AddressBalance = copy(openVolume = PositiveMap((openVolume.xs |-| diff).filter(_._2 != 0)))

  /**
   * @param executionTotalVolumeDiff An order's executed assets or a sum of two
   */
  def withExecuted(txId: Option[ExchangeTransaction.Id], executionTotalVolumeDiff: Map[Asset, Long]): (AddressBalance, Set[Asset]) = {
    val (updated, changedAssets) = pessimisticCorrection.withExecuted(txId, executionTotalVolumeDiff)
    (
      copy(pessimisticCorrection = updated, openVolume = PositiveMap((openVolume.xs |+| executionTotalVolumeDiff).filter(_._2 != 0))),
      changedAssets
    )
  }

  def withObserved(txId: ExchangeTransaction.Id): (AddressBalance, Set[Asset]) = {
    val (updated, changedAssets) = pessimisticCorrection.withObserved(txId)
    (copy(pessimisticCorrection = updated), changedAssets)
  }

}

object AddressBalance {

  val empty = AddressBalance(
    regular = PositiveMap.empty,
    outgoingLeasing = None,
    pessimisticCorrection = AddressPessimisticCorrection(NonPositiveMap.empty, Map.empty, Set.empty),
    openVolume = PositiveMap.empty
  )

}
