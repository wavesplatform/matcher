package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates

/**
 * @param outgoingLeasing Always positive, receive at start
 * @param pessimisticCorrection Always negative, receive at start
 * @param openVolume Always positive, amount of assets those reserved for open orders
 */
case class AddressBalance(
  regular: Map[Asset, Long],
  outgoingLeasing: Option[Long],
  pessimisticCorrection: AddressPessimisticCorrection,
  openVolume: Map[Asset, Long]
) {

  // We count only regular, because openVolume is created from orders and
  //  pessimisticCorrection updated by a stream (so, this asset couldn't be fetched)
  def allAssets: Set[Asset] = regular.keySet

  /**
   * Use this method only if sure, that we known all information about specified assets
   */
  def allTradableBalances: Map[Asset, Long] = tradableBalances(allAssets)

  /**
   * Use this method only if sure, that we known all information about specified assets
   */
  def tradableBalances(assets: Set[Asset]): Map[Asset, Long] =
    assets.foldLeft(Map.empty[Asset, Long]) {
      case (r, asset) =>
        // getOrElse, because we fetch this information at start
        val x =
          regular.getOrElse(asset, 0L) -
          outgoingLeasing.getOrElse(0L) +
          pessimisticCorrection.getBy(asset) -
          openVolume.getOrElse(asset, 0L)

        r.updated(asset, math.max(0L, x))
    }

  def withInit(snapshot: AddressBalanceUpdates): AddressBalance =
    // The original data have a higher precedence, because we receive it from the stream
    // And it is guaranteed to be eventually consistent.
    // Otherwise we can get a stale data and replace the fresh one by it.
    AddressBalance(
      regular = snapshot.regular ++ regular,
      outgoingLeasing = outgoingLeasing.orElse(snapshot.outLease),
      pessimisticCorrection = pessimisticCorrection.withInit(snapshot.pessimisticCorrection),
      openVolume = openVolume
    )

  def withFresh(updates: AddressBalanceUpdates): AddressBalance =
    AddressBalance(
      regular = regular ++ updates.regular,
      outgoingLeasing = updates.outLease.orElse(outgoingLeasing),
      pessimisticCorrection = pessimisticCorrection.withFreshUnconfirmed(updates.pessimisticCorrection),
      openVolume = openVolume
    )

  def withVolume(xs: Map[Asset, Long]): AddressBalance = copy(openVolume = openVolume |+| xs)
  def withoutVolume(xs: Map[Asset, Long]): AddressBalance = copy(openVolume = openVolume |-| xs)

}

object AddressBalance {

  val empty = AddressBalance(
    regular = Map.empty,
    outgoingLeasing = None,
    pessimisticCorrection = AddressPessimisticCorrection(Map.empty, Map.empty, Set.empty),
    openVolume = Map.empty
  )

}
