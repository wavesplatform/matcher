package com.wavesplatform.dex.grpc.integration.clients.state

import cats.Monoid
import com.wavesplatform.dex.collection.MapOps.Ops2
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

case class BlockchainBalance(regular: Map[Address, Map[Asset, Long]], outLeases: Map[Address, Long]) {

  // TODO
  def diffIndex: DiffIndex = DiffIndex(
    regular = regular.view.mapValues(_.keySet).toMap,
    outLeases = outLeases.keySet
  )

  def isEmpty: Boolean = regular.isEmpty && outLeases.isEmpty

}

object BlockchainBalance {

  implicit val blockchainBalanceMonoid = new Monoid[BlockchainBalance] {
    override val empty: BlockchainBalance = BlockchainBalance(Map.empty, Map.empty)

    override def combine(x: BlockchainBalance, y: BlockchainBalance): BlockchainBalance = BlockchainBalance(
      regular = x.regular.deepReplace(y.regular),
      outLeases = x.outLeases ++ y.outLeases
    )

  }

}
