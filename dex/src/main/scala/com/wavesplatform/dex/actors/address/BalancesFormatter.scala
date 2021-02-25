package com.wavesplatform.dex.actors.address

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates

object BalancesFormatter {

  def format(xs: Map[Asset, Long]): String =
    xs.toList.sortBy(_._1.maybeBase58Repr)
      .map { case (asset, v) => s"$v ${asset.maybeBase58Repr.fold("🔷")(_.take(5))}" }
      .mkString("{", ", ", "}")

  def format(xs: AddressBalanceUpdates): String = s"r=${format(xs.regular)}, l=${xs.outgoingLeasing}, p=${format(xs.pessimisticCorrection)}"
}
