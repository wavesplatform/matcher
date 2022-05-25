package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup
import com.wavesplatform.dex.grpc.integration.clients.domain.TransactionWithChanges
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.events.protobuf.StateUpdate

object Implicits {

  implicit final class UtxTransactionOps(val self: UtxTransaction) extends AnyVal {

    def pessimisticPortfolios: AddressAssets = self.diff.flatMap(_.stateUpdate).fold(Map.empty: AddressAssets) {
      _.pessimisticPortfolios
    }

  }

  implicit final class TransactionWithChangesOps(val self: TransactionWithChanges) extends AnyVal {
    def pessimisticPortfolios: AddressAssets = self.changes.pessimisticPortfolios
  }

  // Probably we need to move ByteString -> Address conversion further
  implicit final class StateUpdateOps(val self: StateUpdate) extends AnyVal {

    def pessimisticPortfolios: AddressAssets = {
      // Balances
      val p1 = self.balances.groupBy(_.address).flatMap {
        case (address, updates) =>
          val balances = updates.view
            .flatMap(bu => bu.amountAfter.map(aa => bu.amountBefore -> aa))
            .collect {
              case (before, after) if after.amount - before < 0 =>
                after.assetId.toVanillaAsset -> (after.amount - before) // Count only pessimistic
            }
            .toList
            .foldMap(Map(_))

          if (balances.isEmpty) none
          else (address -> balances).some
      }

      // Leasing
      Monoid
        .combineAll(
          p1 ::
          self.leasingForAddress.view
            .collect { case x if x.outAfter > 0 => Map(x.address -> Map[Asset, Long](Waves -> -x.outAfter)) }
            .toList
        )
        .map { case (address, xs) => address.toVanillaAddress -> xs }
    }

  }

}
