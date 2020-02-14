package com.wavesplatform.dex

import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging

case class AddressStateDiff(changedSpendableAssets: Set[Asset], changedReservableAssets: Set[Asset]) extends ScorexLogging {

  def getAllAssets: Set[Asset]                                  = changedSpendableAssets ++ changedReservableAssets
  def updateReservedAssets(diff: Set[Asset]): AddressStateDiff  = copy(changedReservableAssets = changedReservableAssets ++ diff)
  def updateSpendableAssets(diff: Set[Asset]): AddressStateDiff = copy(changedSpendableAssets = changedSpendableAssets ++ diff)

  def pack(tradable: Map[Asset, Long], reservable: Map[Asset, Long]): WsAddressState = {
    WsAddressState(tradable.map { case (a, tb) => a -> WsBalances(tb, reservable.getOrElse(a, 0)) })
  }
}

object AddressStateDiff {
  def empty: AddressStateDiff = AddressStateDiff(Set.empty, Set.empty)
}
