package com.wavesplatform.dex.grpc.integration.clients.state

import cats.instances.long._
import cats.syntax.group._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainState.ChangedAddresses

case class BlockchainState(
  height: Int,
  changedAddresses: List[ChangedAddresses],
  regularBalances: Map[Address, Map[Asset, Long]],
  outLeases: Map[Address, Long],
  utxExpenses: Map[Address, Map[Asset, Long]]
) {



  def networkBalanceOf(address: Address): Map[Asset, Long] =
    (regularBalances.getOrElse(address, Map.empty) |-|
      outLeases.get(address).fold(Map.empty[Asset, Long])(x => Map(Asset.Waves -> x)) |-|
      utxExpenses.getOrElse(address, Map.empty))
      .filter(_._2 > 0)

}

object BlockchainState {

  case class ChangedAddresses(height: Int, addresses: Set[Address])

}
