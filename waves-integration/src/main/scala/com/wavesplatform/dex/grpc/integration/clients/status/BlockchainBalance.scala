package com.wavesplatform.dex.grpc.integration.clients.status

import cats.Monoid
import com.wavesplatform.dex.collection.MapOps.Ops2D
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

case class BlockchainBalance(regular: Map[Address, Map[Asset, Long]], outLeases: Map[Address, Long]) {

  def diffIndex: DiffIndex = DiffIndex(
    regular = regular.view.mapValues(_.keySet).toMap,
    outLeases = outLeases.keySet
  )

  def filter(by: DiffIndex): BlockchainBalance = BlockchainBalance(
    regular = regular.view
      .map { case (address, assets) => address -> by.regular.get(address).fold(Map.empty[Asset, Long])(assets.view.filterKeys(_).toMap) }
      .filterNot(_._2.isEmpty)
      .toMap,
    outLeases = outLeases.filter { case (address, _) => by.outLeases.contains(address) }
  )

  def isEmpty: Boolean = regular.isEmpty && outLeases.isEmpty

  override def toString: String = s"BlockchainBalance(r=$regular, ol=$outLeases)"
}

object BlockchainBalance {

  // TODO DEX-1002
  implicit val blockchainBalanceMonoid = new Monoid[BlockchainBalance] {
    override val empty: BlockchainBalance = BlockchainBalance(Map.empty, Map.empty)

    override def combine(x: BlockchainBalance, y: BlockchainBalance): BlockchainBalance = BlockchainBalance(
      regular = x.regular.deepReplace(y.regular),
      outLeases = x.outLeases ++ y.outLeases
    )

  }

}
