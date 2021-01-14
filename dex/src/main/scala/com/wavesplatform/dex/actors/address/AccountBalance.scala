package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates

case class AccountBalance(
  allFetched: Boolean,
  regular: Map[Asset, Long],
  outLease: Option[Long], // always positive, receive at start
  pessimisticCorrection: Map[Asset, Long], // always negative, receive at start
  openVolume: Map[Asset, Long]
  // todo OECState
) {

  def fetched: Set[Asset] =
    // We don't add "++ openVolume.keySet", because openVolume is created from orders, thus regular balances were received
    regular.keySet ++ pessimisticCorrection.keySet + Asset.Waves

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
          outLease.getOrElse(throw new RuntimeException("outLease")) +
          pessimisticCorrection.getOrElse(asset, 0L) - // always have this information
          openVolume.getOrElse(asset, 0L)

        r.updated(asset, math.max(0L, x))
    }

  def withProbablyStale(snapshot: AddressBalanceUpdates): AccountBalance =
    // The original data have a higher precedence, because we receive it from the stream
    // And it is guaranteed to be eventually consistent.
    // Otherwise we can get a stale data and replace the fresh one by it.
    AccountBalance(
      allFetched = allFetched,
      regular = snapshot.regular ++ regular,
      outLease = outLease.orElse(snapshot.outLease),
      pessimisticCorrection = snapshot.pessimisticCorrection ++ pessimisticCorrection,
      openVolume = openVolume
    )

  //  E.g. if we receive changes from a stream - it is a high precedence
  //                             by a request - it is a low precedence
  def withFresh(updates: AddressBalanceUpdates): AccountBalance =
    AccountBalance(
      allFetched = allFetched,
      regular = regular ++ updates.regular,
      outLease = updates.outLease.orElse(outLease),
      pessimisticCorrection = pessimisticCorrection ++ updates.pessimisticCorrection,
      openVolume = openVolume
    )

  def withAllFetched: AccountBalance = copy(allFetched = true)

  def withVolume(xs: Map[Asset, Long]): AccountBalance = copy(openVolume = openVolume |+| xs)
  def withoutVolume(xs: Map[Asset, Long]): AccountBalance = copy(openVolume = openVolume |-| xs)

}

object AccountBalance {

  val empty = AccountBalance(
    allFetched = false,
    regular = Map.empty,
    outLease = 0L,
    pessimisticCorrection = Map.empty,
    openVolume = Map.empty
  )

}
