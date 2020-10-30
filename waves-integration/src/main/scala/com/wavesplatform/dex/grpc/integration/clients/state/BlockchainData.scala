package com.wavesplatform.dex.grpc.integration.clients.state

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.collection.MapOps.Ops
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainData.ChangedAddresses

case class BlockchainData(
  blockInfo: BlockInfo,
  changedAddresses: List[ChangedAddresses],
  regularBalances: Map[Address, Map[Asset, Long]],
  outLeases: Map[Address, Long]
) {

  def blockchainBalanceOf(address: Address): Map[Asset, Long] =
    (regularBalances.getOrElse(address, Map.empty) |-|
      outLeases.get(address).fold(Map.empty[Asset, Long])(x => Map(Asset.Waves -> x)))
      .filter(_._2 > 0)

  // Ignore stale or further blocks
  def combine(changes: BlockchainData): BlockchainData =
    if (blockInfo.height + 1 == changes.blockInfo.height) unsafeCombine(changes)
    else this

  def unsafeCombine(changes: BlockchainData): BlockchainData = copy(
    blockInfo = changes.blockInfo,
    changedAddresses = changes.changedAddresses ::: changedAddresses,
    regularBalances = regularBalances.deepReplace(changes.regularBalances),
    outLeases = outLeases ++ changes.outLeases
  )

}

object BlockchainData {
  case class ChangedAddresses(height: Int, addresses: Set[Address])
}
