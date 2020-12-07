package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.Implicits.UtxTransactionOps
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.Amount
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class UtxTransactionOpsTestSuite extends WavesIntegrationSuiteBase with PBEntitiesGen with ScalaCheckDrivenPropertyChecks with NoShrink {

  private val negativeChangesAddress, address1, address2 = mkPBAddress
  private val asset = mkPBIssuedAsset

  "UtxTransactionOps" - {
    "pessimisticPortfolio" - {
      "counts only spending" in {
        val changes = List(
          (address1, pbWaves, 10L),
          (address1, asset, 0L),
          (negativeChangesAddress, pbWaves, -20L),
          (negativeChangesAddress, asset, -21L)
        )

        // omit the transaction field, because it doesn't participate
        val tx = mkUtxTransaction(
          id = mkPBTxId,
          balanceUpdates = changes.map { case (address, asset, v) =>
            StateUpdate.BalanceUpdate(
              address = address,
              amount = Amount(asset, v).some
            )
          }
        )

        tx.pessimisticPortfolio should matchTo(Map[Address, Map[Asset, Long]](
          negativeChangesAddress.toVanillaAddress -> Map(
            Asset.Waves -> -20L,
            asset.toVanillaAsset -> -21L
          )
        ))
      }

      "counts out leases" in {
        val changes = List(
          (address1, -10L),
          (address2, 0L),
          (negativeChangesAddress, 21L)
        )

        val tx = mkUtxTransaction(
          id = mkPBTxId,
          leaseUpdates = changes.map { case (address, v) =>
            StateUpdate.LeasingUpdate(
              address = address,
              out = v
            )
          }
        )

        tx.pessimisticPortfolio should matchTo(Map[Address, Map[Asset, Long]](
          negativeChangesAddress.toVanillaAddress -> Map(Asset.Waves -> -21L)
        ))
      }

      "sum out leases and spending for Waves" in {
        val balanceUpdateChanges = List(
          (address1, pbWaves, 10L),
          (address1, asset, 0L),
          (negativeChangesAddress, pbWaves, -20L),
          (negativeChangesAddress, asset, -21L)
        )

        val leasingChanges = List(
          (address1, -10L),
          (address2, 0L),
          (negativeChangesAddress, 22L)
        )

        val tx = mkUtxTransaction(
          id = mkPBTxId,
          balanceUpdates = balanceUpdateChanges.map { case (address, asset, v) =>
            StateUpdate.BalanceUpdate(
              address = address,
              amount = Amount(asset, v).some
            )
          },
          leaseUpdates = leasingChanges.map { case (address, v) =>
            StateUpdate.LeasingUpdate(
              address = address,
              out = v
            )
          }
        )

        tx.pessimisticPortfolio should matchTo(Map[Address, Map[Asset, Long]](
          negativeChangesAddress.toVanillaAddress -> Map(
            Asset.Waves -> -42L,
            asset.toVanillaAsset -> -21L
          )
        ))
      }

      "properties" - {
        "only negative changes" in forAll(pbUtxTransactionGen) { tx =>
          tx.pessimisticPortfolio.foreach { case (address, assets) =>
            withClue(s"$address: ") {
              assets.foreach { case (asset, v) =>
                withClue(s"$asset: ") {
                  v should be < 0L
                }
              }
            }
          }
        }
      }
    }
  }

}
