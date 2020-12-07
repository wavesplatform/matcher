package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

object Implicits {

  // Probably we need to move ByteString -> Address conversion further
  implicit final class UtxTransactionOps(val self: UtxTransaction) extends AnyVal {

    // TODO DEX-995 Could we do it faster? TODO test
    def pessimisticPortfolio: AddressAssets = self.diff.flatMap(_.stateUpdate)
      .fold(Map.empty[Address, Map[Asset, Long]]) { diff =>
        // Balances
        val p1 = diff.balances.groupBy(_.address).flatMap {
          case (address, updates) =>
            val balances = updates.view
              .flatMap(_.amount)
              .collect {
                case x if x.amount < 0 => x.assetId.toVanillaAsset -> x.amount // Count only pessimistic
              }
              .toMap

            if (balances.isEmpty) none
            else (address.toVanillaAddress -> balances).some
        }

        // Leasing
        diff.leases.foldLeft(p1) {
          case (r, x) =>
            if (x.out <= 0) r // Ignore an invalid values
            else {
              val address = x.address.toVanillaAddress
              val orig = r.getOrElse(address, Map.empty)
              val updated = orig.updated(Waves, orig.getOrElse(Waves, 0L) - x.out)
              r.updated(address, updated)
            }
        }
      }

  }

}
