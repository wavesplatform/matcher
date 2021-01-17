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
  allFetched: Boolean,
  regular: Map[Asset, Long],
  outgoingLeasing: Option[Long],
  pessimisticCorrection: AddressPessimisticCorrection,
  openVolume: Map[Asset, Long]
) {

  // We count only regular, because openVolume is created from orders and
  //  pessimisticCorrection updated by a stream (so, this asset couldn't be fetched)
  def fetched: Set[Asset] = regular.keySet

  // TODO always used with tradableBalances?
  def needFetch(assets: Set[Asset]): Set[Asset] =
    if (allFetched) Set.empty
    else // We don't check pessimisticCorrection, because we always have the actual information
      assets.filter {
        case asset: Asset.IssuedAsset => !regular.contains(asset)
        case Asset.Waves => false // Always pre-fetch
      }

  /**
   * Use this method only if sure, that we known all information about specified assets
   */
  def allTradableBalances: Map[Asset, Long] = tradableBalances(fetched)

  /**
   * Use this method only if sure, that we known all information about specified assets
   */
  def tradableBalances(assets: Set[Asset]): Map[Asset, Long] =
    assets.foldLeft(Map.empty[Asset, Long]) {
      case (r, asset) =>
        val x =
          regular.getOrElse(asset, throw new RuntimeException(s"regular($asset)")) -
          outgoingLeasing.getOrElse(throw new RuntimeException("outLease")) +
          pessimisticCorrection.getBy(asset) - // we always have this information
          openVolume.getOrElse(asset, 0L)

        r.updated(asset, math.max(0L, x))
    }

  def withProbablyStale(snapshot: AddressBalanceUpdates): AddressBalance =
    // The original data have a higher precedence, because we receive it from the stream
    // And it is guaranteed to be eventually consistent.
    // Otherwise we can get a stale data and replace the fresh one by it.
    AddressBalance(
      allFetched = allFetched,
      regular = snapshot.regular ++ regular,
      outgoingLeasing = outgoingLeasing.orElse(snapshot.outLease),
      pessimisticCorrection = pessimisticCorrection.withProbablyStaleUnconfirmed(snapshot.pessimisticCorrection),
      openVolume = openVolume
    )

  //  E.g. if we receive changes from a stream - it is a high precedence
  //                             by a request - it is a low precedence
  def withFresh(updates: AddressBalanceUpdates): AddressBalance =
    AddressBalance(
      allFetched = allFetched,
      regular = regular ++ updates.regular,
      outgoingLeasing = updates.outLease.orElse(outgoingLeasing),
      pessimisticCorrection = pessimisticCorrection.withFreshUnconfirmed(updates.pessimisticCorrection),
      openVolume = openVolume
    )

  def withAllFetched: AddressBalance = copy(allFetched = true)

  def withVolume(xs: Map[Asset, Long]): AddressBalance = copy(openVolume = openVolume |+| xs)
  def withoutVolume(xs: Map[Asset, Long]): AddressBalance = copy(openVolume = openVolume |-| xs)

}

object AddressBalance {

  val empty = AddressBalance(
    allFetched = false,
    regular = Map.empty,
    outgoingLeasing = None,
    pessimisticCorrection = AddressPessimisticCorrection(Map.empty, Map.empty, Set.empty),
    openVolume = Map.empty
  )

}
