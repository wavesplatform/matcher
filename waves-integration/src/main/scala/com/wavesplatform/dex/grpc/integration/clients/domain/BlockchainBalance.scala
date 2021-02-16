package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.collections.MapOps.Ops2D
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

// TODO DEX-1058
/**
 * @param outgoingLeasing Are always positive
 */
case class BlockchainBalance(regular: Map[Address, Map[Asset, Long]], outgoingLeasing: Map[Address, Long]) {

  def diffIndex: DiffIndex = DiffIndex(
    regular = regular.view.mapValues(_.keySet).toMap,
    outgoingLeasing = outgoingLeasing.keySet
  )

  def filter(by: DiffIndex): BlockchainBalance = BlockchainBalance(
    regular = regular.view
      .map { case (address, assets) => address -> by.regular.get(address).fold(Map.empty[Asset, Long])(assets.view.filterKeys(_).toMap) }
      .filterNot(_._2.isEmpty)
      .toMap,
    outgoingLeasing = outgoingLeasing.filter { case (address, _) => by.outgoingLeasing.contains(address) }
  )

  def isEmpty: Boolean = regular.isEmpty && outgoingLeasing.isEmpty

  override def toString: String =
    s"BlockchainBalance(r={${regular.keys.map(_.stringRepr.take(5)).mkString(", ")}}, ol={${outgoingLeasing.keys.map(_.stringRepr.take(5)).mkString(", ")}})"

}

object BlockchainBalance {

  // TODO DEX-1002
  implicit val blockchainBalanceMonoid = new Monoid[BlockchainBalance] {
    override val empty: BlockchainBalance = BlockchainBalance(Map.empty, Map.empty)

    override def combine(x: BlockchainBalance, y: BlockchainBalance): BlockchainBalance = BlockchainBalance(
      regular = x.regular.deepReplace(y.regular),
      outgoingLeasing = x.outgoingLeasing ++ y.outgoingLeasing
    )

  }

}
